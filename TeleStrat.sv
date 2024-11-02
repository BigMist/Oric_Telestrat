//============================================================================
//  Oric Telestrat
//  rampa@encomix.org
//
//  Tape loader by JAson-A and Flandango.
//============================================================================
`default_nettype none

module guest_top
  #
  (
   parameter  TOT_DISKS = 1
   )
(
	input         CLOCK_27,
`ifdef USE_CLOCK_50
	input         CLOCK_50,
`endif

	output        LED,
	output [VGA_BITS-1:0] VGA_R,
	output [VGA_BITS-1:0] VGA_G,
	output [VGA_BITS-1:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,

`ifdef USE_HDMI
	output        HDMI_RST,
	output  [7:0] HDMI_R,
	output  [7:0] HDMI_G,
	output  [7:0] HDMI_B,
	output        HDMI_HS,
	output        HDMI_VS,
	output        HDMI_PCLK,
	output        HDMI_DE,
	inout         HDMI_SDA,
	inout         HDMI_SCL,
	input         HDMI_INT,
`endif

	input         SPI_SCK,
	inout         SPI_DO,
	input         SPI_DI,
	input         SPI_SS2,    // data_io
	input         SPI_SS3,    // OSD
	input         CONF_DATA0, // SPI_SS for user_io

`ifdef USE_QSPI
	input         QSCK,
	input         QCSn,
	inout   [3:0] QDAT,
`endif
`ifndef NO_DIRECT_UPLOAD
	input         SPI_SS4,
`endif

	output [12:0] SDRAM_A,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nWE,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nCS,
	output  [1:0] SDRAM_BA,
	output        SDRAM_CLK,
	output        SDRAM_CKE,

`ifdef DUAL_SDRAM
	output [12:0] SDRAM2_A,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_DQML,
	output        SDRAM2_DQMH,
	output        SDRAM2_nWE,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nCS,
	output  [1:0] SDRAM2_BA,
	output        SDRAM2_CLK,
	output        SDRAM2_CKE,
`endif

	output        AUDIO_L,
	output        AUDIO_R,
`ifdef I2S_AUDIO
	output        I2S_BCK,
	output        I2S_LRCK,
	output        I2S_DATA,
`endif
`ifdef I2S_AUDIO_HDMI
	output        HDMI_MCLK,
	output        HDMI_BCK,
	output        HDMI_LRCK,
	output        HDMI_SDATA,
`endif
`ifdef SPDIF_AUDIO
	output        SPDIF,
`endif
`ifdef USE_AUDIO_IN
	input         AUDIO_IN,
`endif
	input         UART_RX,
	output        UART_TX

);

`ifdef NO_DIRECT_UPLOAD
localparam bit DIRECT_UPLOAD = 0;
wire SPI_SS4 = 1;
`else
localparam bit DIRECT_UPLOAD = 1;
`endif

`ifdef USE_QSPI
localparam bit QSPI = 1;
assign QDAT = 4'hZ;
`else
localparam bit QSPI = 0;
`endif

`ifdef VGA_8BIT
localparam VGA_BITS = 8;
`else
localparam VGA_BITS = 6;
`endif

`ifdef USE_HDMI
localparam bit HDMI = 1;
assign HDMI_RST = 1'b1;
`else
localparam bit HDMI = 0;
`endif

`ifdef BIG_OSD
localparam bit BIG_OSD = 1;
`define SEP "-;",
`else
localparam bit BIG_OSD = 0;
`define SEP
`endif

`ifdef USE_AUDIO_IN
localparam bit USE_AUDIO_IN = 1;
wire TAPE_SOUND=AUDIO_IN;
`else
localparam bit USE_AUDIO_IN = 0;
wire TAPE_SOUND=UART_RX;
`endif


`include "build_id.v"
localparam CONF_STR = {
	"TeleStrat;;",
	"S0U,DSKIMG,Mount Drive A:;",
//	"S1U,DSKIMG,Mount Drive B:;",
	"F2,ROM,Load Cartridge;",
	"O1,BANK4,ROM,RAM;",
	"-;",
   "F1,TAP,Load TAP file;",
	"OAB,Tape Audio,Mute,Low,High;",
	"OC,Tape Input,File,ADC;",
	"TD,Rewind;",
	"-;",
	"O45,Scandoubler Fx,None,CRT 25%,CRT 50%,CRT 75%;",
	"O89,Stereo,Off,ABC (West Europe),ACB (East Europe);",
	"-;",
	"T0,Reset & Apply;",
	"V,v",`BUILD_DATE
};


/////////////////////// CLOCKS ////////////////////////////

wire        clk_sys;
wire        clk_acia;
wire        clk_hdmi;
wire        locked;


pll pll
(
	.inclk0(CLOCK_50),
	.c0(clk_sys),
	.c1(clk_acia),
	.c2(clk_hdmi),
	.locked(locked)
);


reg        reset = 0;
reg [16:0] clr_addr = 0;

always @(posedge clk_sys) begin

	if(~&clr_addr) clr_addr <= clr_addr + 1'd1;
	else reset <= 0;

	if(status[0] | buttons[1]) begin
		clr_addr <= 0;
		reset <= 1;
	end
	
end

//////////////////// HPS ///////////////////////////////


wire [15:0] joy0;
wire [15:0] joy1;
wire  [4:0] joy_t0={joy0[3],joy0[2],joy0[4],joy0[1],joy0[0]}; //Up,Down,Fire,Left,Right
wire  [4:0] joy_t1={joy1[3],joy1[2],joy1[4],joy1[1],joy1[0]};
wire        fire2_t1=joy1[5]; 
wire        fire3_t1=joy1[6]; 
wire        forced_scandoubler;
wire [127:0] status;

wire [31:0] sd_lba[TOT_DISKS];
wire  [TOT_DISKS-1:0] sd_rd;
wire  [TOT_DISKS-1:0] sd_wr;
wire  [TOT_DISKS-1:0] sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din[TOT_DISKS];
wire        sd_buff_wr;
wire        sd_ack_conf;
wire        sd_conf;
wire        sd_sdhc = 1'b1;

wire  [TOT_DISKS-1:0] img_mounted;
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
wire [64:0] rtc;



`ifdef USE_HDMI
wire        i2c_start;
wire        i2c_read;
wire  [6:0] i2c_addr;
wire  [7:0] i2c_subaddr;
wire  [7:0] i2c_dout;
wire  [7:0] i2c_din;
wire        i2c_ack;
wire        i2c_end;
`endif

wire  [1:0] buttons, switches;
wire			ypbpr;
wire        scandoubler_disable;
wire        key_pressed;
wire [7:0]  key_code;
wire        key_strobe;
wire        key_extended;


user_io #(.STRLEN($size(CONF_STR)>>3), .SD_IMAGES(TOT_DISKS), .FEATURES(32'h0 | (BIG_OSD << 13) | (HDMI << 14))) user_io
(	
	.clk_sys        	(clk_sys         	),
	.clk_sd           (clk_sys           ),
	.conf_str       	(CONF_STR       	),
	.SPI_CLK        	(SPI_SCK        	),
	.SPI_SS_IO      	(CONF_DATA0     	),
	.SPI_MISO       	(SPI_DO        	),
	.SPI_MOSI       	(SPI_DI         	),

`ifdef USE_HDMI
	.i2c_start      (i2c_start      ),
	.i2c_read       (i2c_read       ),
	.i2c_addr       (i2c_addr       ),
	.i2c_subaddr    (i2c_subaddr    ),
	.i2c_dout       (i2c_dout       ),
	.i2c_din        (i2c_din        ),
	.i2c_ack        (i2c_ack        ),
	.i2c_end        (i2c_end        ),
`endif

	.buttons        	(buttons        	),
	.switches       	(switches      	),
	.scandoubler_disable (scandoubler_disable	),
	.ypbpr          	(ypbpr          	),
	.key_strobe     	(key_strobe     	),
	.key_pressed    	(key_pressed    	),
	.key_extended   	(key_extended   	),
	.key_code       	(key_code       	),
	.joystick_0       ( joy0      ),
	.joystick_1       ( joy1      ),
	.status         	(status         	),
	// SD CARD
   .sd_lba                      (sd_lba[0]     ),
	.sd_rd                       (sd_rd         ),
	.sd_wr                       (sd_wr         ),
	.sd_ack                      (sd_ack        ),
	.sd_ack_conf                 (sd_ack_conf   ),
	.sd_conf                     (sd_conf       ),
	.sd_sdhc                     (sd_sdhc       ),
	.sd_dout                     (sd_buff_dout  ),
	.sd_dout_strobe              (sd_buff_wr    ),
	.sd_din                      (sd_buff_din[0]),
//	.sd_din_strobe               (sd_din_strobe ),
	.sd_buff_addr                (sd_buff_addr  ),
	.img_mounted                 (img_mounted   ),
	.img_size                    (img_size      )
);

data_io data_io(
	.clk_sys       ( clk_sys       ),
	.SPI_SCK       ( SPI_SCK       ),
	.SPI_SS2       ( SPI_SS2       ),
	.SPI_DI        ( SPI_DI        ),
	.clkref_n      ( 1'b0          ),
	.ioctl_download( ioctl_download),
	.ioctl_index   ( ioctl_index   ),
	.ioctl_wr      ( ioctl_wr      ),
	.ioctl_addr    ( ioctl_addr    ),
	.ioctl_dout    ( ioctl_dout    )
);


//////////////////////// ROM CARTRIDGE ////////////

wire[15:0] rom_address;
wire[7:0]  rom_data;
reg [1:0]  rom_pages;

wire cart_download = ioctl_index==2 && ioctl_download;

cart  cart
(
  .clock      (clk_sys), 
  .address    (cart_download ? ioctl_addr: rom_address),
  .data       (ioctl_dout),
  .wren       (cart_download? ioctl_wr : 1'b0),
  .q          (rom_data)
);

///////////////////////////////////////////////////



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
wire [23:0]  cpu_ad;
wire [7:0]   cpu_do;

telestrat telestrat
(
	.clk_in           (clk_sys),
	.RESET            (reset),
	.clk_acia         (clk_acia),
	
	.key_pressed      (key_pressed),
	.key_code         (key_code),
	.key_extended     (key_extended),
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
	
//	.RTC              (rtc),

	.ram_ad           (ram_ad),
	.ram_d            (ram_d),
	.ram_q            (ram_q),
	.ram_cs           (ram_cs),
	.ram_oe           (),
	.ram_we           (ram_we),
	
	.rom_ad           (rom_address),
	.rom_q            (rom_data),
	.bank4            (status[1]),
	
	
	
	.joystick_0       (joy_t0),
	.joystick_1       (joy_t1),
	.fire2_t1         (fire2_t1),
	.fire3_t1         (fire3_t1),
	
	.cpu_ad           (cpu_ad),
	.cpu_do           (cpu_do),
   .WD_REn           (WD_REn),
	.WD_WEn           (WD_WEn), 
   .WD_DRQ           (fdd_drq),
	.WD_IRQ           (fdd_irq),
	.WD_HLD           (fdd_hld),
	.WD_CLK           (WD_CLK),
	.WD_RESET         (WD_RESET),
	.FDC_DAL_OUT      (fdc_dal_out),
	.CS1793n          (CS1793n),
	.SSEL             (SSEL),
	.DS0              (DS0),
	.DS1              (DS1),
	.DS2              (DS2),
	.DS3              (DS3),
	
	.pll_locked       (locked),
	.UART_TXD         (UART_TX),
	.UART_RXD         (UART_RX),
	.UART_CTS         (),
	.UART_RTS         ()
	
);


/////////////////// VIDEO PROCESSING ////////////////////////////////



//video_mixer #(.LINE_LENGTH(250), .HALF_DEPTH(1), .GAMMA(1)) video_mixer
//(
//	.*,
//	.R({4{r}}),
//	.G({4{g}}),
//	.B({4{b}}),	
//	.freeze_sync(),
//	.hq2x(scale==1)
//);

	mist_video #(.COLOR_DEPTH(1), .SD_HCNT_WIDTH(11), .OUT_COLOR_DEPTH(VGA_BITS),.USE_BLANKS(1), .BIG_OSD(BIG_OSD)) mist_video (	
	.clk_sys      (clk_sys     ),
	.SPI_SCK      (SPI_SCK    ),
	.SPI_SS3      (SPI_SS3    ),
	.SPI_DI       (SPI_DI     ),
	.R            (r          ),
	.G            (g          ),
	.B            (b          ),
	.HSync        ( hs        ),
	.VSync        ( vs        ),	
	.HBlank       ( HBlank    ),
	.VBlank       ( VBlank    ),	
	.VGA_R        (VGA_R      ),
	.VGA_G        (VGA_G      ),
	.VGA_B        (VGA_B      ),
	.VGA_VS       (VGA_VS     ),
	.VGA_HS       (VGA_HS     ),
	.ce_divider   (1'b0       ),
	.scandoubler_disable(scandoubler_disable),
	.scanlines    (status[5:4]),
	.ypbpr        (ypbpr      )
	);

`ifdef USE_HDMI

  i2c_master #(28_000_000) i2c_master (
	.CLK         (clk_hdmi),
	.I2C_START   (i2c_start),
	.I2C_READ    (i2c_read),
	.I2C_ADDR    (i2c_addr),
	.I2C_SUBADDR (i2c_subaddr),
	.I2C_WDATA   (i2c_dout),
	.I2C_RDATA   (i2c_din),
	.I2C_END     (i2c_end),
	.I2C_ACK     (i2c_ack),

	//I2C bus
	.I2C_SCL     (HDMI_SCL),
	.I2C_SDA     (HDMI_SDA)
);

mist_video #(.COLOR_DEPTH(1), .SD_HCNT_WIDTH(11), .OUT_COLOR_DEPTH(8), .USE_BLANKS(1), .BIG_OSD(BIG_OSD), .VIDEO_CLEANER(1)) hdmi_video (
	.clk_sys     ( clk_hdmi   ),

	.SPI_SCK     ( SPI_SCK    ),
	.SPI_SS3     ( SPI_SS3    ),
	.SPI_DI      ( SPI_DI     ),

	.scanlines   ( status[5:4]  ),

	.ce_divider  ( 3'd1       ),

	.scandoubler_disable ( scandoubler_disable ),
	.no_csync    ( 1'b1       ),
	.ypbpr       ( 1'b0       ),
	.rotate      ( 2'b00      ),
	.blend       ( 1'b0       ),

	.R           ( r ),
	.G           ( g ),
	.B           ( b ),

	.HBlank      ( HBlank      ),
	.VBlank      ( VBlank      ),
	.HSync       ( hs      ),
	.VSync       ( vs      ),

	.VGA_R       ( HDMI_R      ),
	.VGA_G       ( HDMI_G      ),
	.VGA_B       ( HDMI_B      ),
	.VGA_VS      ( HDMI_VS     ),
	.VGA_HS      ( HDMI_HS     ),
	.VGA_DE      ( HDMI_DE     )
);
assign HDMI_PCLK = clk_hdmi;

`endif


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

wire [15:0] dac_in_l,dac_in_r;

assign dac_in_l = psg_l + tapeAudio;
assign dac_in_r = psg_r + tapeAudio;


dac #(
   .c_bits	(16))
audiodac_l(
   .clk_i	(clk_sys	),
   .res_n_i	(1	),
   .dac_i	(dac_in_l),
   .dac_o	(AUDIO_L)
  );

dac #(
   .c_bits	(16))
audiodac_r(
   .clk_i	(clk_sys	),
   .res_n_i	(1	),
   .dac_i	(dac_in_r),
   .dac_o	(AUDIO_R)
  );

wire [31:0] clk_rate =  32'd24_000_000;

`ifdef I2S_AUDIO
i2s i2s (
	.reset(1'b0),
	.clk(clk_sys),
	.clk_rate(clk_rate),

	.sclk(I2S_BCK),
	.lrclk(I2S_LRCK),
	.sdata(I2S_DATA),

	.left_chan ({~dac_in_l[15],dac_in_l[14:0]}),
	.right_chan({~dac_in_r[15],dac_in_r[14:0]})
);
`ifdef I2S_AUDIO_HDMI
assign HDMI_MCLK = 0;
always @(posedge clk_sys) begin
	HDMI_BCK <= I2S_BCK;
	HDMI_LRCK <= I2S_LRCK;
	HDMI_SDATA <= I2S_DATA;
end
`endif
`endif

`ifdef SPDIF_AUDIO
spdif spdif (
	.clk_i(clk_sys),
	.rst_i(1'b0),
	.clk_rate_i(clk_rate),
	.spdif_o(SPDIF),
	.sample_i({dac_in_r, dac_in_l})
);
`endif

//////////////////////// TAPE LOADING ///////////////////////////

wire [1:0] tapeVolume  = status[11:10];
wire       tapeUseADC = status[12];
wire       tapeRewind = status[13];
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

wire        load_tape = ioctl_download && ioctl_index[4:0] == 1;
reg  [15:0] tape_end;
reg         tape_loaded = 1'b0;
reg         ioctl_downlD;


wire [15:0] tape_addr;
wire [7:0]  tape_data;


spram #(.address_width(16)) tapecache (
  .clock(clk_sys),

  .address(load_tape ? ioctl_addr: tape_addr),
  .data(ioctl_dout),
  .wren(ioctl_wr && load_tape),
  .q(tape_data)
);


always @(posedge clk_sys) begin
 if (load_tape) tape_end <= ioctl_addr[15:0];
end

always @(posedge clk_sys) begin
	ioctl_downlD <= load_tape;
	if(~ioctl_downlD & load_tape) tape_loaded <= 1'b1;
end

cassette cassette (
  .clk(clk_sys),
  .reset(reset),
  .rewind(tapeRewind | (load_tape)),
  .en(cas_relay && tape_loaded && ~tapeUseADC), 
  .tape_addr(tape_addr),
  .tape_data(tape_data),

  .tape_end(tape_end),
  .data(casdout)
);



assign tape_in = tapeUseADC ? TAPE_SOUND : casdout;

/////////////////////// FLOPPY DISK ///////////////////////////

wire       wp = img_readonly;
wire       CS1793n;
wire       DS0,DS1,DS2,DS3;
wire       SSEL;
wire       WD_REn,WD_WEn;
wire       WD_CLK;
wire       WD_RESET;

reg [3:0] fdc_sel;
always @(posedge clk_sys) begin
   case({DS3,DS2,DS1,DS0})
	 4'b0001: fdc_sel=0;
	 4'b0010: fdc_sel=1;
	 4'b0100: fdc_sel=2;
	 4'b1000: fdc_sel=3;
	endcase
end

wire [7:0] fdc_dal_out;
wire       fdd_prepare;
wire       fdd_irq;
wire       fdd_drq;
wire       fdd_led;
wire       fdd_hld;

reg [TOT_DISKS-1:0] fdd_ready;
reg [TOT_DISKS-1:0] old_mounted;

genvar                fdd_i;
generate
  for (fdd_i = 0; fdd_i < TOT_DISKS; fdd_i++) begin : g_IM
		
	always @(posedge clk_sys) begin
	   old_mounted[fdd_i] <= img_mounted[fdd_i];
    	if(reset) fdd_ready[fdd_i] <= 1'b1;
	     else if(~old_mounted[fdd_i] & img_mounted[fdd_i]) fdd_ready[fdd_i] <= |img_size;
	end
 end
endgenerate


wd17xx #(.EDSK(1),.MODEL(3),.CLK_EN(24000),.TOT_DISKS(TOT_DISKS)) fdd1
(
	.clk_sys(clk_sys),
	.ce     (WD_CLK),
	.reset  (reset),
	.io_en  (~CS1793n),
	.rd     (~WD_REn),
	.wr     (~WD_WEn),
	.addr   (cpu_ad[1:0]),
	.din    (cpu_do),
	.dout   (fdc_dal_out),
	.intrq  (fdd_irq),
	.drq    (fdd_drq),

	.img_mounted (img_mounted),
	.img_size    (img_size[20:0]),
	.sd_lba      (sd_lba),
	.sd_rd       (sd_rd),
	.sd_wr       (sd_wr),
	.sd_ack      (sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din (sd_buff_din),
	.sd_buff_wr (sd_buff_wr),

	.wp        (wp),
	.size_code (3'b101),
	.layout    (0),
	.side      (SSEL),
	.ready     (1),//(fdd_ready),
	.prepare   (fdd_prepare),
	.busy      (fdd_led),
	.hld       (),
	.fdd_sel   (fdc_sel)
);

assign fdd_hld = img_mounted;

endmodule


