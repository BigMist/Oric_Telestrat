//============================================================================
//  Oric Telestrat
//  rampa@encomix.org
//
//  Tape loader by JAson-A and Flandango.
//============================================================================


module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [48:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	//if VIDEO_ARX[12] or VIDEO_ARY[12] is set then [11:0] contains scaled size instead of aspect ratio.
	output [12:0] VIDEO_ARX,
	output [12:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output [1:0]  VGA_SL,
	output        VGA_SCALER, // Force VGA scaler

	input  [11:0] HDMI_WIDTH,
	input  [11:0] HDMI_HEIGHT,
	output        HDMI_FREEZE,

`ifdef MISTER_FB
	// Use framebuffer in DDRAM
	// FB_FORMAT:
	//    [2:0] : 011=8bpp(palette) 100=16bpp 101=24bpp 110=32bpp
	//    [3]   : 0=16bits 565 1=16bits 1555
	//    [4]   : 0=RGB  1=BGR (for 16/24/32 modes)
	//
	// FB_STRIDE either 0 (rounded to 256 bytes) or multiple of pixel size (in bytes)
	output        FB_EN,
	output  [4:0] FB_FORMAT,
	output [11:0] FB_WIDTH,
	output [11:0] FB_HEIGHT,
	output [31:0] FB_BASE,
	output [13:0] FB_STRIDE,
	input         FB_VBL,
	input         FB_LL,
	output        FB_FORCE_BLANK,

`ifdef MISTER_FB_PALETTE
	// Palette control for 8bit modes.
	// Ignored for other video modes.
	output        FB_PAL_CLK,
	output  [7:0] FB_PAL_ADDR,
	output [23:0] FB_PAL_DOUT,
	input  [23:0] FB_PAL_DIN,
	output        FB_PAL_WR,
`endif
`endif

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	// I/O board button press simulation (active high)
	// b[1]: user button
	// b[0]: osd button
	output  [1:0] BUTTONS,

	input         CLK_AUDIO, // 24.576 MHz
	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)

	//ADC
	inout   [3:0] ADC_BUS,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..6 - USR2..USR6
	// Set USER_OUT to 1 to read from USER_IN.
	output        USER_OSD,
	output  [1:0] USER_MODE,
	input   [7:0] USER_IN,
	output  [7:0] USER_OUT,
	input   [7:0] USER_IN2,
	output  [7:0] USER_OUT2,

	input         OSD_STATUS
);

assign USER_OUT = '1;
assign USER_OUT2 = '1;

assign {UART_RTS, UART_TXD, UART_DTR} = 'Z;
assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;
assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = '0; 
 
assign LED_USER  = ioctl_download | led_disk | tape_adc_act;
assign LED_DISK  = 0;
assign LED_POWER = 0;
assign BUTTONS   = 0; 
assign VGA_SCALER= 0;

assign AUDIO_S   = 0;
assign AUDIO_MIX = 0;

wire [1:0] ar = status[14:13];
video_freak video_freak
(
	.*,
	.VGA_DE_IN(VGA_DE),
	.VGA_DE(),

	.ARX((!ar) ? 12'd4 : (ar - 1'd1)),
	.ARY((!ar) ? 12'd3 : 12'd0),
	.CROP_SIZE(0),
	.CROP_OFF(0),
	.SCALE(status[16:15])
);

`include "build_id.v"
localparam CONF_STR = {
	"TeleStrat;UART9600;",
	"S0,DSKIMG,Mount Drive A:;",
   "F1,TAP,Load TAP file;",
	"-;",
	"O3,CART,HyperBasic,StratOric;",
	"O7,Drive Write,Allow,Prohibit;",
	"-;",
	"O[51:50],Tape Audio,Mute,Low,High;",
	"O[52],Tape Input,File,ADC;",
	"h0T[53],Rewind;",
	"-;",
	"ODE,Aspect ratio,Original,Full Screen,[ARC1],[ARC2];",
	"OAC,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%;",
	"OFG,Scale,Normal,V-Integer,Narrower HV-Integer,Wider HV-Integer;",
	"-;",
	"O89,Stereo,Off,ABC (West Europe),ACB (East Europe);",
	"-;",
	"R0,Reset & Apply;",
	"V,v",`BUILD_DATE
};


/////////////////////// CLOCKS ////////////////////////////

wire        clk_sys;
wire        clk_acia;
wire        locked;


pll pll
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(clk_sys),
	.outclk_1(clk_acia),
	.locked(locked)
);

assign CLK_VIDEO = clk_sys;

reg        reset = 0;
reg [16:0] clr_addr = 0;

always @(posedge clk_sys) begin

	if(~&clr_addr) clr_addr <= clr_addr + 1'd1;
	else reset <= 0;

	if(RESET | status[0] | buttons[1]) begin
		clr_addr <= 0;
		reset <= 1;
	end
	
end

//////////////////// HPS ///////////////////////////////

wire [10:0] ps2_key;

wire [15:0] joy0;
wire [15:0] joy1;
wire  [1:0] buttons;
wire        forced_scandoubler;
wire [127:0] status;

wire [31:0] sd_lba[2];
wire  [1:0] sd_rd;
wire  [1:0] sd_wr;
wire  [1:0] sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din[2];
wire        sd_buff_wr;
wire  [1:0] img_mounted;
wire [31:0] img_size;
wire        img_readonly;

wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        ioctl_download;
wire  [7:0] ioctl_index;

reg         status_set;
reg  [31:0] status_out;

wire [21:0] gamma_bus;
wire        freeze_sync;

hps_io #(.CONF_STR(CONF_STR), .VDNUM(2)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.ps2_key(ps2_key),

	.joystick_0(joy0),
	.joystick_1(joy1),
	.buttons(buttons),
	.forced_scandoubler(forced_scandoubler),
	.status(status),

	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr),
	.img_mounted(img_mounted),
	.img_size(img_size),
	.img_readonly(img_readonly),

	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),
	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.gamma_bus(gamma_bus)
);


///////////////////////////////////////////////////

wire key_strobe = old_keystb ^ ps2_key[10];
reg old_keystb = 0;
always @(posedge clk_sys) old_keystb <= ps2_key[10];


wire  [11:0] psg_a;
wire  [11:0] psg_b;
wire  [11:0] psg_c;
wire  [13:0] psg_out;

wire  [1:0] stereo = status [9:8];

wire        r, g, b; 
wire        hs, vs, HBlank, VBlank;
wire        clk_pix;


wire [15:0] ram_ad;
wire  [7:0] ram_d;
wire        ram_we,ram_cs;

reg   [7:0] ram[65536];
always @(posedge clk_sys) begin
	if(reset) ram[clr_addr[15:0]] <= '1;
	else if(ram_we & ram_cs) ram[ram_ad] <= ram_d;
end

wire  [7:0] ram_q;
always @(posedge clk_sys) ram_q <= ram[ram_ad];

wire        led_disk;

telestrat telestrat
(
	.clk_in           (clk_sys),
	.RESET            (reset),
	.clk_acia         (clk_acia),
	
	.key_pressed      (ps2_key[9]),
	.key_code         (ps2_key[7:0]),
	.key_extended     (ps2_key[8]),
	.key_strobe       (key_strobe),
	.PSG_OUT_A        (psg_a),
	.PSG_OUT_B        (psg_b),
	.PSG_OUT_C        (psg_c),
	.PSG_OUT          (psg_out),
	.VIDEO_CLK			(clk_pix),
	.VIDEO_R				(r),
	.VIDEO_G				(g),
	.VIDEO_B				(b),
	.VIDEO_HSYNC		(hs),
	.VIDEO_VSYNC		(vs),
	.VIDEO_HBLANK		(HBlank),
	.VIDEO_VBLANK		(VBlank),

	.K7_TAPEIN        (tape_in ),
	.K7_TAPEOUT       (tape_out),
	.K7_REMOTE        (cas_relay),

	.ram_ad           (ram_ad),
	.ram_d            (ram_d),
	.ram_q            (ram_q),
	.ram_cs           (ram_cs),
	.ram_oe           (),
	.ram_we           (ram_we),
	.joystick_0       (joy0),
	.joystick_1       (joy1),
	.fd_led           (led_disk),
	.fdd_ready        (fdd_ready),
	.fdd_busy         (),
	.fdd_reset        (0),
	.fdd_layout       (0),
	.phi2             (),
	.pll_locked       (locked),
	.disk_enable      (1),
	//
	.UART_TXD         (UART_TXD),
	.UART_RXD         (UART_RXD),
	.UART_CTS         (UART_CTS),
	.UART_RTS         (UART_RTS),
	//
	.rom			      (~rom),
	.img_mounted      (img_mounted[0]), // signaling that new image has been mounted
	.img_size         (img_size[31:0]), // size of image in bytes
	.img_wp           (status[7] | img_readonly), // write protect
   .sd_lba           (sd_lba[0]),
	.sd_rd            (sd_rd[0]),
	.sd_wr            (sd_wr[0]),
	.sd_ack           (sd_ack[0]),
	.sd_buff_addr     (sd_buff_addr),
	.sd_buff_dout     (sd_buff_dout),
	.sd_buff_din      (sd_buff_din[0]),
	.sd_buff_wr       (sd_buff_wr)
);

reg  fdd_ready = 0;
always @(posedge clk_sys) if(img_mounted[0]) fdd_ready <= |img_size;

reg rom = 0;
always @(posedge clk_sys) if(reset) rom <= ~status[3];


/////////////////// VIDEO PROCESSING ////////////////////////////////

reg clk_pix2;
always @(posedge clk_sys) clk_pix2 <= clk_pix;

reg ce_pix;
always @(posedge CLK_VIDEO) begin
	reg old_clk;
	
	old_clk <= clk_pix2;
	ce_pix <= ~old_clk & clk_pix2;
end

reg HSync, VSync;
always @(posedge CLK_VIDEO) begin
	if(ce_pix) begin
		HSync <= ~hs;
		if(~HSync & ~hs) VSync <= ~vs;
	end
end

wire [2:0] scale = status[12:10];
wire [2:0] sl = scale ? scale - 1'd1 : 3'd0;
wire       scandoubler = scale || forced_scandoubler;

assign VGA_F1 = 0;
assign HDMI_FREEZE = 0;
assign VGA_SL = sl[1:0];


video_mixer #(.LINE_LENGTH(250), .HALF_DEPTH(1), .GAMMA(1)) video_mixer
(
	.*,
	.R({4{r}}),
	.G({4{g}}),
	.B({4{b}}),	
	.freeze_sync(),
	.hq2x(scale==1)
);

////////////////////// AUDIO MIXING /////////////////////////////
wire [15:0] psg_l;
wire [15:0] psg_r;
reg [12:0] psg_ab;
reg [12:0] psg_ac;
reg [12:0] psg_bc;
wire [7:0] tapeAudio;


assign tapeAudio = {|tapeVolume ? (tapeVolume == 2'd1 ? {1'b0,tape_in} : {tape_in,1'b0} ) : 2'b00,6'b00};



always @ (clk_sys,psg_a,psg_b,psg_c) begin
 psg_ab <= {{1'b0,psg_a} + {1'b0,psg_b}};
 psg_ac <= {{1'b0,psg_a} + {1'b0,psg_c}};
 psg_bc <= {{1'b0,psg_b} + {1'b0,psg_c}};
end

assign psg_l = (stereo == 2'b00) ? {psg_out,2'b0} : (stereo == 2'b01) ? {psg_ab,3'b0}: {psg_ac,3'b0};
assign psg_r = (stereo == 2'b00) ? {psg_out,2'b0} : (stereo == 2'b01) ? {psg_bc,3'b0}: {psg_bc,3'b0};

assign AUDIO_L = psg_l + tapeAudio;
assign AUDIO_R = psg_r + tapeAudio;

//////////////////////// TAPE LOADING ///////////////////////////

wire [1:0] tapeVolume  = status[51:50];
wire       tapeUseADC = status[52];
wire       tapeRewind = status[53];
wire       tape_in;
wire       tape_out;

wire tape_clk;
always @(posedge clk_sys) begin
	if (reset)
    	tape_clk <= 1'b0;
	else
    	tape_clk <= ~tape_clk;	
end

wire casdout;
wire cas_relay;

wire        load_tape = ioctl_index==1;
reg  [15:0] tape_end;
reg         tape_loaded = 1'b0;
reg         ioctl_downlD;

//Tape Writting - Not Implemented ATM - Flandango
//reg         tape_wr;
//reg [7:0]   tape_dout;


wire [15:0] tape_addr;
wire [7:0]  tape_data;


spram #(.address_width(16)) tapecache (
  .clock(clk_sys),

  .address((ioctl_index == 1 && ioctl_download) ? ioctl_addr: tape_addr),
  .data(ioctl_dout),
  .wren(ioctl_wr && load_tape),
  .q(tape_data)
);


always @(posedge clk_sys) begin
 if (load_tape) tape_end <= ioctl_addr[15:0];
end

always @(posedge clk_sys) begin
	ioctl_downlD <= ioctl_download;
	if(ioctl_downlD & ~ioctl_download) tape_loaded <= 1'b1;
end

cassette cassette (
  .clk(clk_sys),
  .reset(reset),
  .rewind(tapeRewind | (load_tape && ioctl_download)),
  .en(cas_relay && tape_loaded && ~tapeUseADC), 
  .tape_addr(tape_addr),
  .tape_data(tape_data),

  .tape_end(tape_end),
  .data(casdout)
);

wire tape_adc, tape_adc_act;
ltc2308_tape ltc2308_tape
(
	.clk(CLK_50M),
	.ADC_BUS(ADC_BUS),
	.dout(tape_adc),
	.active(tape_adc_act)
);

assign tape_in = tapeUseADC ? tape_adc : casdout;


endmodule
