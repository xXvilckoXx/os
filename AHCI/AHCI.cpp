#include <hal.h>
#include <string.h>
#include "AHCI.h"
#include "DebugDisplay.h"

#define ATA_DEV_BUSY 0x80
#define ATA_DEV_DRQ 0x08


enum {
	FIS_TYPE_REG_H2D	= 0x27,	// Register FIS - host to device
	FIS_TYPE_REG_D2H	= 0x34,	// Register FIS - device to host
	FIS_TYPE_DMA_ACT	= 0x39,	// DMA activate FIS - device to host
	FIS_TYPE_DMA_SETUP	= 0x41,	// DMA setup FIS - bidirectional
	FIS_TYPE_DATA		= 0x46,	// Data FIS - bidirectional
	FIS_TYPE_BIST		= 0x58,	// BIST activate FIS - bidirectional
	FIS_TYPE_PIO_SETUP	= 0x5F,	// PIO setup FIS - device to host
	FIS_TYPE_DEV_BITS	= 0xA1,	// Set device bits FIS - device to host
};


int find_cmdslot(HBA_PORT *port) {
	// If not set in SACT and CI, the slot is free
	uint32_t slots = (port->sact | port->ci);
	for (int i=0; i<5; i++)
	{
		if ((slots&1) == 0)
			return i;
		slots >>= 1;
	}
	return -1;
}


bool read(HBA_PORT *port, uint32_t startl, uint32_t starth, uint32_t count, uint16_t *buf){
	port->is = (uint32_t) -1;		// Clear pending interrupt bits
	int slot = find_cmdslot(port);
	if (slot == -1)
		return false;

	//create CMD header
	HBA_CMD_HEADER *cmdheader = (HBA_CMD_HEADER*)port->clb;
	cmdheader += slot;

	cmdheader->cfl = sizeof(FIS_REG_H2D)/sizeof(uint32_t);	// Command FIS size
	cmdheader->w = 0;		// Read from device
	cmdheader->prdtl = (uint16_t)((count-1)>>4) + 1;	// PRDT entries count
														// every entry will read 16 sectors

	//create CMD  Table
	HBA_CMD_TBL *cmdtbl  = (HBA_CMD_TBL*) ((uint64_t) cmdheader->ctbau << 32 | cmdheader->ctba);
	memset(cmdtbl, 0, sizeof(HBA_CMD_TBL) +
			((cmdheader->prdtl-1)*sizeof(HBA_PRDT_ENTRY)));
	
	
	// 8K bytes (16 sectors) per PRDT
	int i;
	for (i = 0; i < cmdheader->prdtl-1; i++){
		cmdtbl->prdt_entry[i].dba = (uint32_t) buf;
		cmdtbl->prdt_entry[i].dbc = 8*1024-1;	// 8K bytes (this value should always be set to 1 less than the actual value)
		cmdtbl->prdt_entry[i].i = 1;			//notice how we set the same value to both dbc and move the buffer
		buf += 4*1024;	// 4K words				//the buffer is 16 bit for value so adding 1 to buffer pointer is
		count -= 16;	// 16 sectors			//actilly add 16 bit to the pointer or 2 bytes. buf += X  <==> dbc = 2*X
												//same for count, sector is 512 bytes so devide the dbc by 512 to get count
												//remember to add 1 to dbc before any calculation here.
	}
	
	
	// Last entry
	cmdtbl->prdt_entry[i].dba = (uint32_t) buf;
	cmdtbl->prdt_entry[i].dbc = (count<<9)-1;	//the remainder of count * 512 bytes per sector
	cmdtbl->prdt_entry[i].i = 1;

	int a = 1;
	a++;
	a *= 52;

	for (int i=0, a=0, b=0, c=0, d=0, u=0; i < 52; i++){
		a++;
		b++;
		c++;
		d++;
		u++;
	}

	/*
	// Create Fis
	FIS_REG_H2D *cmdfis = (FIS_REG_H2D*)(&cmdtbl->cfis);
	cmdfis->fis_type = FIS_TYPE_REG_H2D;
	cmdfis->c = 1;	// Command
	cmdfis->command = 0x25; //ATA_CMD_READ_DMA_EX

	cmdfis->lba0 = (uint8_t)startl;
	cmdfis->lba1 = (uint8_t)(startl>>8);
	cmdfis->lba2 = (uint8_t)(startl>>16);
	cmdfis->device = 1<<6;	// LBA mode

	cmdfis->lba3 = (uint8_t)(startl>>24);
	cmdfis->lba4 = (uint8_t)starth;
	cmdfis->lba5 = (uint8_t)(starth>>8);

	cmdfis->countl = count & 0xFF;
	cmdfis->counth = (count >> 8) & 0xFF;

	//waiting for port to be ready before send command
	int spin = 0; // Spin lock timeout counter
	while ((port->tfd & (ATA_DEV_BUSY | ATA_DEV_DRQ)) && spin < 1000000)
	{
		spin++;
	}	if (spin == 1000000) {
		return false;
	}

	port->ci = (uint32_t) 1<<slot;	// Issue command
	*/
	return true;
}