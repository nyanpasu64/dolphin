// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include "Common/CommonTypes.h"
#include "Common/MathUtil.h"
#include "Common/FileUtil.h"

#include "Core/DSP/DSPAccelerator.h"
#include "Core/DSP/DSPCore.h"
#include "Core/DSP/DSPHost.h"
#include "Core/DSP/DSPHWInterface.h"
#include "Core/DSP/DSPInterpreter.h"

#include <fstream>

#include <set>
#include <vector>
#include <tuple>

#include "Common/Logging/Log.h"

std::ofstream logfile;

using std::set;
using std::vector;
using std::tuple;
using std::fstream;

typedef tuple<vector<s16>, u16, u16, u16, u32, u8> adpcm_byte;

set<adpcm_byte> written, buffered;

template <typename T>
void insertVec(vector<u8> &vec, T num) {
	vec.insert(vec.end(), (u8*)&num, (u8*)(&num + 1));
}

template <typename T>
void insertVec(vector<u8> &vec, T* arr, size_t len) {
	vec.insert(vec.end(), (u8*)arr, (u8*)(arr + len));
}

// The hardware adpcm decoder :)
static s16 ADPCM_Step(u32& _rSamplePos)
{
	// Open log file.
	if (!logfile.is_open())
	{
		logfile.open(File::GetUserPath(D_DUMPAUDIO_IDX) + "adpcm_dump.bin", fstream::out | fstream::binary | fstream::trunc);
	}

	const s16 *pCoefTable = (const s16 *)&g_dsp.ifx_regs[DSP_COEF_A1_0];

	if ((_rSamplePos & 15) == 0)
	{
		g_dsp.ifx_regs[DSP_PRED_SCALE] = DSPHost::ReadHostMemory((_rSamplePos & ~15) >> 1);
		_rSamplePos += 2;
	}

	int scale = 1 << (g_dsp.ifx_regs[DSP_PRED_SCALE] & 0xF);
	int coef_idx = (g_dsp.ifx_regs[DSP_PRED_SCALE] >> 4) & 0x7;

	s32 coef1 = pCoefTable[coef_idx * 2 + 0];
	s32 coef2 = pCoefTable[coef_idx * 2 + 1];

	int temp = (_rSamplePos & 1) ?
	       (DSPHost::ReadHostMemory(_rSamplePos >> 1) & 0xF) :
	       (DSPHost::ReadHostMemory(_rSamplePos >> 1) >> 4);

	if (temp >= 8)
		temp -= 16;

	// 0x400 = 0.5  in 11-bit fixed point
	int val = (scale * temp) + ((0x400 + coef1 * (s16)g_dsp.ifx_regs[DSP_YN1] + coef2 * (s16)g_dsp.ifx_regs[DSP_YN2]) >> 11);

	MathUtil::Clamp(&val, -0x7FFF, 0x7FFF);

	// ********
	// LOG OUTPUT
	// ********

	if (_rSamplePos % 2 == 0) {
		adpcm_byte new_data = adpcm_byte(
			vector<s16>(pCoefTable, pCoefTable + 16),
			g_dsp.ifx_regs[DSP_YN1],
			g_dsp.ifx_regs[DSP_YN2],
			g_dsp.ifx_regs[DSP_PRED_SCALE],
			_rSamplePos >> 1,
			DSPHost::ReadHostMemory(_rSamplePos >> 1)
			);

		if (written.find(new_data) == written.end()) {
			// buffered.insert(new_data);
			written.insert(new_data);

			vector<u8> log_bytes;

			// 32 BYTES
			insertVec(log_bytes, pCoefTable, 16);

			// 6 BYTES
			insertVec(log_bytes, g_dsp.ifx_regs[DSP_YN1]);
			insertVec(log_bytes, g_dsp.ifx_regs[DSP_YN2]);
			insertVec(log_bytes, g_dsp.ifx_regs[DSP_PRED_SCALE]);

			// 4 BYTES
			insertVec(log_bytes, _rSamplePos >> 1);

			// 1 BYTE
			insertVec(log_bytes, DSPHost::ReadHostMemory(_rSamplePos >> 1));

			logfile.write((char*)&(log_bytes[0]), log_bytes.size());

			// Flushing slows it down. You have to quit before the file can be read, even without flushing. (Windows)
			// So flushing slows it down and doesn't allow you to read the data any faster anyway.
			// logfile.flush();
			// log_bytes now contains 43 bytes.
			// ERROR_LOG(AUDIO, "%d", log_bytes.size());
		}
	}

	g_dsp.ifx_regs[DSP_YN2] = g_dsp.ifx_regs[DSP_YN1];
	g_dsp.ifx_regs[DSP_YN1] = val;

	_rSamplePos++;

	// The advanced interpolation (linear, polyphase,...) is done by the ucode,
	// so we don't need to bother with it here.
	return val;
}

u16 dsp_read_aram_d3()
{
	// Zelda ucode reads ARAM through 0xffd3.
	const u32 EndAddress = (g_dsp.ifx_regs[DSP_ACEAH] << 16) | g_dsp.ifx_regs[DSP_ACEAL];
	u32 Address = (g_dsp.ifx_regs[DSP_ACCAH] << 16) | g_dsp.ifx_regs[DSP_ACCAL];
	u16 val = 0;

	switch (g_dsp.ifx_regs[DSP_FORMAT])
	{
		case 0x5:   // u8 reads
			val = DSPHost::ReadHostMemory(Address);
			Address++;
			break;
		case 0x6:   // u16 reads
			val = (DSPHost::ReadHostMemory(Address * 2) << 8) | DSPHost::ReadHostMemory(Address * 2 + 1);
			Address++;
			break;
		default:
			ERROR_LOG(DSPLLE, "dsp_read_aram_d3() - unknown format 0x%x", g_dsp.ifx_regs[DSP_FORMAT]);
			break;
	}

	if (Address >= EndAddress)
	{
		// Set address back to start address. (never seen this here!)
		Address = (g_dsp.ifx_regs[DSP_ACSAH] << 16) | g_dsp.ifx_regs[DSP_ACSAL];
	}

	g_dsp.ifx_regs[DSP_ACCAH] = Address >> 16;
	g_dsp.ifx_regs[DSP_ACCAL] = Address & 0xffff;
	return val;
}

void dsp_write_aram_d3(u16 value)
{
	// Zelda ucode writes a bunch of zeros to ARAM through d3 during
	// initialization.  Don't know if it ever does it later, too.
	// Pikmin 2 Wii writes non-stop to 0x10008000-0x1000801f (non-zero values too)
	// Zelda TP Wii writes non-stop to 0x10000000-0x1000001f (non-zero values too)
	u32 Address = (g_dsp.ifx_regs[DSP_ACCAH] << 16) | g_dsp.ifx_regs[DSP_ACCAL];

	switch (g_dsp.ifx_regs[DSP_FORMAT])
	{
		case 0xA:   // u16 writes
			DSPHost::WriteHostMemory(value >> 8, Address * 2);
			DSPHost::WriteHostMemory(value & 0xFF, Address * 2 + 1);
			Address++;
			break;
		default:
			ERROR_LOG(DSPLLE, "dsp_write_aram_d3() - unknown format 0x%x", g_dsp.ifx_regs[DSP_FORMAT]);
			break;
	}

	g_dsp.ifx_regs[DSP_ACCAH] = Address >> 16;
	g_dsp.ifx_regs[DSP_ACCAL] = Address & 0xffff;
}

u16 dsp_read_accelerator()
{
	const u32 EndAddress = (g_dsp.ifx_regs[DSP_ACEAH] << 16) | g_dsp.ifx_regs[DSP_ACEAL];
	u32 Address = (g_dsp.ifx_regs[DSP_ACCAH] << 16) | g_dsp.ifx_regs[DSP_ACCAL];
	u16 val;
	u8 step_size_bytes = 0;

	// let's do the "hardware" decode DSP_FORMAT is interesting - the Zelda
	// ucode seems to indicate that the bottom two bits specify the "read size"
	// and the address multiplier.  The bits above that may be things like sign
	// extension and do/do not use ADPCM.  It also remains to be figured out
	// whether there's a difference between the usual accelerator "read
	// address" and 0xd3.
	switch (g_dsp.ifx_regs[DSP_FORMAT])
	{
		case 0x00:  // ADPCM audio
			if ((EndAddress & 15) == 0)
				step_size_bytes = 1;
			else
				step_size_bytes = 2;
			val = ADPCM_Step(Address);
			break;
		case 0x0A:  // 16-bit PCM audio
			val = (DSPHost::ReadHostMemory(Address * 2) << 8) | DSPHost::ReadHostMemory(Address * 2 + 1);
			g_dsp.ifx_regs[DSP_YN2] = g_dsp.ifx_regs[DSP_YN1];
			g_dsp.ifx_regs[DSP_YN1] = val;
			step_size_bytes = 2;
			Address++;
			break;
		case 0x19:  // 8-bit PCM audio
			val = DSPHost::ReadHostMemory(Address) << 8;
			g_dsp.ifx_regs[DSP_YN2] = g_dsp.ifx_regs[DSP_YN1];
			g_dsp.ifx_regs[DSP_YN1] = val;
			step_size_bytes = 2;
			Address++;
			break;
		default:
			ERROR_LOG(DSPLLE, "dsp_read_accelerator() - unknown format 0x%x", g_dsp.ifx_regs[DSP_FORMAT]);
			step_size_bytes = 2;
			Address++;
			val = 0;
			break;
	}

	// TODO: Take GAIN into account
	// adpcm = 0, pcm8 = 0x100, pcm16 = 0x800
	// games using pcm8 : Phoenix Wright Ace Attorney (WiiWare), Megaman 9-10 (WiiWare)
	// games using pcm16: GC Sega games, ...

	// Check for loop.
	// Somehow, YN1 and YN2 must be initialized with their "loop" values,
	// so yeah, it seems likely that we should raise an exception to let
	// the DSP program do that, at least if DSP_FORMAT == 0x0A.
	if (Address == (EndAddress + step_size_bytes - 1))
	{
		// Set address back to start address.
		Address = (g_dsp.ifx_regs[DSP_ACSAH] << 16) | g_dsp.ifx_regs[DSP_ACSAL];
		DSPCore_SetException(EXP_ACCOV);
	}

	g_dsp.ifx_regs[DSP_ACCAH] = Address >> 16;
	g_dsp.ifx_regs[DSP_ACCAL] = Address & 0xffff;
	return val;
}
