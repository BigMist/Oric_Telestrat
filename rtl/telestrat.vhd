--
-- A simulation model of ORIC TELESTRAT hardware
-- Based on the ORIC ATMOS Core (initial  Copyright (c) SEILEBOST - March 2006)
--
--

  library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_arith.all;
  use ieee.std_logic_unsigned.all;

  
entity telestrat is
  port (
    CLK_IN            : in    std_logic;
	 CLK_ACIA          : in    std_logic;
    RESET             : in    std_logic;
	 key_pressed       : in    std_logic;
	 key_extended      : in    std_logic;
	 key_code          : in    std_logic_vector(7 downto 0);
	 key_strobe        : in    std_logic;
    K7_TAPEIN         : in    std_logic;
    K7_TAPEOUT        : out   std_logic;
    K7_REMOTE         : out   std_logic;
--	 RTC               : in    std_logic_vector(63 downto 0);
	 PSG_OUT           : out   unsigned(13 downto 0);
    PSG_OUT_A         : out   unsigned(11 downto 0);
    PSG_OUT_B         : out   unsigned(11 downto 0);
    PSG_OUT_C         : out   unsigned(11 downto 0);
    VIDEO_R           : out   std_logic;
    VIDEO_G           : out   std_logic;
    VIDEO_B           : out   std_logic;
    VIDEO_HSYNC       : out   std_logic;
    VIDEO_VSYNC       : out   std_logic;
    VIDEO_VBLANK      : out   std_logic;
    VIDEO_HBLANK      : out   std_logic;
	 VIDEO_CLK         : out  std_logic;
	 ram_ad            : out std_logic_vector(15 downto 0);
	 ram_d             : out std_logic_vector( 7 downto 0);
	 ram_q             : in  std_logic_vector( 7 downto 0);
	 ram_cs            : out std_logic;
	 ram_oe            : out std_logic;
	 ram_we            : out std_logic;

	 rom_ad            : out std_logic_vector(15 downto 0);
	 rom_q             : in  std_logic_vector( 7 downto 0);
	 bank4             : in std_logic;
	 
	 cpu_ad            : buffer std_logic_vector(23 downto 0);
	 cpu_do            : buffer std_logic_vector( 7 downto 0);
	 
	 WD_REn            : buffer std_logic;
	 WD_WEn            : buffer std_logic; 
    WD_DRQ            : in std_logic;
	 WD_IRQ            : in std_logic;
	 WD_HLD            : in std_logic;
	 WD_CLK            : buffer std_logic;
	 FDC_DAL_OUT       : in std_logic_vector( 7 downto 0);
	 CS1793n           : buffer std_logic;
	 SSEL              : out std_logic;
	 DS0               : out std_logic;
	 DS1               : out std_logic;
	 DS2               : out std_logic;
	 DS3               : out std_logic;
	 WD_RESET          : out std_logic;
	 
	 phi2              : out std_logic;
	 fdd_ready         : in std_logic;
	 joystick_0        : in std_logic_vector( 4 downto 0);
	 joystick_1        : in std_logic_vector( 4 downto 0);
	 fire2_t1          : in std_logic;
	 fire3_t1          : in std_logic;
	 pll_locked        : in std_logic;
	 disk_enable       : in std_logic;
	 uart_txd          : out std_logic;
	 uart_rxd          : in std_logic;
	 uart_rts          : out std_logic;
	 uart_cts          : in std_logic
	 );
end;

architecture RTL of telestrat is
  
    -- Gestion des resets
	 signal RESETn        		: std_logic;
    signal reset_dll_h        : std_logic;
    signal delay_count        : std_logic_vector(7 downto 0) := (others => '0');
    signal clk_cnt            : std_logic_vector(2 downto 0) := "000";

    -- cpu
    --signal cpu_ad             : std_logic_vector(23 downto 0);
    signal cpu_di             : std_logic_vector(7 downto 0);
	 signal cpu_di_last        : std_logic_vector(7 downto 0);
    --signal cpu_do             : std_logic_vector(7 downto 0);
    signal cpu_rw             : std_logic;
    signal via1_irq           : std_logic;
	 signal via2_irq           : std_logic;
    signal via1_irq_n         : std_logic;
	 signal via2_irq_n         : std_logic;

	 signal cpu_irq            : std_logic;
      
	-- VIA 1
	SIGNAL via1_pa_out_oe : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via1_pa_out_oe_l : STD_LOGIC_VECTOR(7 DOWNTO 0);

	SIGNAL via1_pa_in : STD_LOGIC_VECTOR(7 DOWNTO 0)  ;
	SIGNAL via1_pa_out : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via1_pa_in_from_psg : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via1_ca2_out : STD_LOGIC;
	SIGNAL via1_cb1_out : STD_LOGIC;
	SIGNAL via1_cb1_oe_l : STD_LOGIC;
	SIGNAL via1_cb2_out : STD_LOGIC;
	SIGNAL via1_cb2_oe_l : STD_LOGIC;
	SIGNAL via1_pb_in : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via1_pb_out : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via1_pb_oe_l : STD_LOGIC_VECTOR(7 DOWNTO 0); 
	SIGNAL VIA1_DO : STD_LOGIC_VECTOR(7 DOWNTO 0);

	-- VIA 2
		
	SIGNAL via2_pa_out_oe : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via2_pa_in : STD_LOGIC_VECTOR(7 DOWNTO 0) ;
	SIGNAL via2_pa_out : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via2_pa_in_from_psg : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via2_cb1_out : STD_LOGIC;
	SIGNAL via2_cb1_in  : STD_LOGIC;
	SIGNAL via2_cb1_oe_l : STD_LOGIC;
	SIGNAL via2_cb2_out : STD_LOGIC;
	SIGNAL via2_cb2_oe_l : STD_LOGIC;
	SIGNAL via2_pb_in : STD_LOGIC_VECTOR(7 DOWNTO 0) ;
	SIGNAL via2_pb_out: STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL via2_pb_out_oe : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL VIA2_DO : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL VIA2_DO_OE : STD_LOGIC;
	-- CS 
	SIGNAL CS300n : STD_LOGIC;
	SIGNAL CS314n : STD_LOGIC;
	SIGNAL CS318n : STD_LOGIC;
	SIGNAL CS31Cn : STD_LOGIC;
	SIGNAL CS320n : STD_LOGIC;
	SIGNAL CS360n : STD_LOGIC;
	SIGNAL CS0n   : STD_LOGIC;
	SIGNAL CS1n   : STD_LOGIC;
	SIGNAL CS2n   : STD_LOGIC;
	SIGNAL CS3n   : STD_LOGIC;
	SIGNAL CS4n   : STD_LOGIC;
	SIGNAL CS5n   : STD_LOGIC;
	SIGNAL CS6n   : STD_LOGIC;
	SIGNAL ROM_CSn: STD_LOGIC;
	-- ACIA
	signal ACIA_DO: STD_LOGIC_VECTOR(7 downto 0);
	signal acia_irq: STD_LOGIC;
	signal XTLI    : STD_LOGIC;
	signal RXD    : STD_LOGIC;
	signal TXD    : STD_LOGIC;
	signal CTS    : STD_LOGIC;
  	signal RTS    : STD_LOGIC;
	signal DTR    : STD_LOGIC;
	
   -- Controller
	 signal cont_D_OUT         : std_logic_vector(7 downto 0);
	 signal cont_irq           : std_logic;

    -- Keyboard PS2
    signal KEY_HIT            : std_logic;
    signal KEYB_RESETn        : std_logic;
    signal KEYB_NMIn          : std_logic;
	 SIGNAL KEY_ROW : STD_LOGIC_VECTOR(7 DOWNTO 0);

    -- PSG
    signal ym_ioa_out         : std_logic_vector (7 downto 0);
    signal psg_do             : std_logic_vector (7 downto 0);
    signal psg_bidir          : std_logic;
	 signal ym_o_ioa           : std_logic_vector (7 downto 0);

    -- ULA 1
    signal ula_phi2           : std_logic;
    signal ula_CSIOn          : std_logic;
    --signal ula_CSROMn         : std_logic;
	 signal ula_CSRAMn         : std_logic;
    signal ula_AD_SRAM        : std_logic_vector(15 downto 0);
    signal ula_CE_SRAM        : std_logic;
    signal ula_OE_SRAM        : std_logic;
    signal ula_WE_SRAM        : std_logic;
	 signal ula_LATCH_SRAM     : std_logic;
	 signal ula_MAPn           : std_logic;
    signal ula_CLK_4          : std_logic;
    signal ula_CLK_4_en       : std_logic;
 	 signal ENA_1MHZ           : std_logic;
	 signal ENA_1MHZ_N         : std_logic;

	 -- RAM
	 signal RAM_BANK0_DO       :std_logic_vector(7 downto 0);
	 signal RAM_BANK1_DO       :std_logic_vector(7 downto 0);
	 signal RAM_BANK2_DO       :std_logic_vector(7 downto 0);
	 signal RAM_BANK3_DO       :std_logic_vector(7 downto 0);
	 signal RAM_BANK4_DO       :std_logic_vector(7 downto 0);
	 --- Printer port
	 signal PRN_STROBE			: std_logic;
	 signal PRN_DATA           : std_logic_vector(7 downto 0);


	 signal SRAM_DO            : std_logic_vector(7 downto 0);
	 
	 signal swnmi           	: std_logic;
	 signal swrst              : std_logic;
	 signal joy_mux            : std_logic_vector(4 downto 0);
	 
	 
		 
COMPONENT keyboard
	PORT
	(
		clk_sys      : IN STD_LOGIC;
		key_pressed  : IN STD_LOGIC;
		key_extended : IN STD_LOGIC;
		key_strobe   : IN STD_LOGIC;
		key_code     : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		row          : IN STD_LOGIC_VECTOR(2 DOWNTO 0);
		col          : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
		key_hit      : OUT STD_LOGIC;
		swnmi        : OUT STD_LOGIC;
		swrst        : OUT STD_LOGIC
	);
END COMPONENT;

COMPONENT psg
PORT (
	  clock : IN STD_LOGIC;
	  ce    : IN STD_LOGIC;
	  reset : IN STD_LOGIC;
	  bdir : IN STD_LOGIC;
	  bc1 : IN STD_LOGIC;
	  sel : IN STD_LOGIC;
	  d   : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
	  q   : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

	  ioad : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
	  ioaq : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
	  iobd : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
	  iobq : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);

	  MIX : OUT UNSIGNED (13 DOWNTO 0);
	  A   : OUT UNSIGNED (11 DOWNTO 0);
	  B   : OUT UNSIGNED(11 DOWNTO 0);
	  C   : OUT UNSIGNED(11 DOWNTO 0)
);
END COMPONENT;


COMPONENT oric_ula is
  Port (
    CLK_24 : in  std_logic;              -- 12 MHz clock
    nRESET : in  std_logic;              -- Asynchronous reset
    AH     : in  std_logic_vector(15 downto 0); -- Address bits 15:8 from 6502
    D      : in  std_logic_vector(7 downto 0); -- Data bus
    RnW    : in  std_logic;              -- 6502 read signal indicator
    nMAP   : in  std_logic;              -- From expansion bus
    RC     : out std_logic_vector(15 downto 0); -- Row/column address to 4164 RAMs
    MXSEL  : out std_logic;              -- DRAM mux select
    nROM   : out std_logic;              -- ROM chip select
	 nRAM   : out std_logic;              -- RAM chip select
	 SRAM_CE   : out std_logic;              -- RAM chip select
    PHI    : out std_logic;              -- Output clock
	 PHI_EN: out std_logic;
    PHI_EN_N: out std_logic;
    CAS    : out std_logic;              -- DRAM Column Access Strobe, active high
    RAS    : out std_logic;              -- DRAM Row Access Strobe, active high
    WE     : out std_logic;              -- DRAM Write Enable, active high
    nIO    : out std_logic;              -- 6522 chip select
    SYNC   : out std_logic;              -- Composite sync signal
	 HSYNC  : out std_logic;
	 VSYNC  : out std_logic;
	 HBLANK : out std_logic;
	 VBLANK : out std_logic;
    RED    : out std_logic;              -- Red video output
    GREEN  : out std_logic;              -- Green video output
    BLUE   : out std_logic               -- Blue video output
  );
end COMPONENT;

begin

cpu_irq <=  via1_irq_n and via2_irq_n  and cont_irq and acia_irq;

inst_cpu : entity work.T65
	port map (
		Mode    		=> "00",
      Res_n   		=> RESETn,
      Enable  		=> ENA_1MHZ_N,
      Clk     		=> CLK_IN,
      Rdy     		=> '1',
      Abort_n 		=> '1',
      IRQ_n   		=> cpu_irq,
      NMI_n   		=> '1',
      SO_n    		=> '1',
      R_W_n   		=> cpu_rw,
      A       		=> cpu_ad,
      DI      		=> cpu_di,
      DO      		=> cpu_do
);

ram_ad  <= ula_AD_SRAM when (ula_PHI2 = '0') else cpu_ad(15 downto 0);
ram_d   <= cpu_do;
SRAM_DO <= ram_q;
ram_cs  <= '0' when RESETn = '0' else ula_CE_SRAM;
ram_we  <= '0' when RESETn = '0' else ula_WE_SRAM;
phi2    <= ula_PHI2;

WD_RESET <= not RESETn;

inst_ram1 : entity work.ram16k
	port map (
		clock			=> CLK_IN,
		address		=> cpu_ad(13 downto 0),
		data        => cpu_do,
		wren        => not CS0n and not cpu_rw, 
		q 			   => RAM_BANK1_DO
);
inst_ram2 : entity work.ram16k
	port map (
		clock			=> CLK_IN,
		address		=> cpu_ad(13 downto 0),
		data        => cpu_do,
		wren        => not CS1n and not cpu_rw, 
		q 			   => RAM_BANK2_DO
);
inst_ram3 : entity work.ram16k
	port map (
		clock			=> CLK_IN,
		address		=> cpu_ad(13 downto 0),
		data        => cpu_do,
		wren        => not CS2n and not cpu_rw, 
		q 			   => RAM_BANK3_DO
);

inst_ram4 : entity work.ram16k
	port map (
		clock			=> CLK_IN,
		address		=> cpu_ad(13 downto 0),
		data        => cpu_do,
		wren        => not CS3n and not cpu_rw, 
		q 			   => RAM_BANK4_DO
);

via1_pa_out_oe_l <= not via1_pa_out_oe;
via1_irq_n <= not via1_irq;
via2_irq_n <= not via2_irq;

inst_via1 : entity work.via6522
	port map
	(
		clock           => CLK_IN,
		rising          => ENA_1MHZ,
		falling         => ENA_1MHZ_N,
		reset           => not RESETn,

		addr            => cpu_ad(3 downto 0),
		wen             => not cpu_rw  and  not CS300n,
		ren             => cpu_rw and not CS300n,
		data_in         => cpu_do,
		data_out        => VIA1_DO,

		port_a_i        => via1_pa_in,
		port_a_o        => via1_pa_out,
		port_a_t        => via1_pa_out_oe,

		port_b_i        => via1_pb_in,
		port_b_o        => via1_pb_out,
		port_b_t        => open,

		ca1_i           => '1',

		ca2_o           => via1_ca2_out,
		ca2_i           => '1',
		ca2_t           => open,

		cb1_i           => K7_TAPEIN,
		cb1_o           => via1_cb1_out,
		cb1_t           => open,

		cb2_i           => '1',
		cb2_o           => via1_cb2_out,
		cb2_t           => open,

		irq             => via1_irq
	);

inst_via2 : entity work.via6522
	port map
	(
		clock           => CLK_IN,
		rising          => ENA_1MHZ,
		falling         => ENA_1MHZ_N,
		reset           => not RESETn,

		addr            => cpu_ad(3 downto 0),
		wen             => not cpu_rw  and  not CS320n,
		ren             => cpu_rw and not CS320n,
		data_in         => cpu_do,
		data_out        => VIA2_DO,

		port_a_i        => via2_pa_in,
		port_a_o        => via2_pa_out,
		port_a_t        => via2_pa_out_oe,

		port_b_i        => via2_pb_in,
		port_b_o        => via2_pb_out,
		port_b_t        => open,

		ca1_i           => '1',
		ca2_o           => open,
		ca2_i           => '1',
		ca2_t           => open,

		cb1_i           => '1',
		cb1_o           => open,
		cb1_t           => open,

		cb2_i           => '1',
		cb2_o           => UART_TXD,
		cb2_t           => open,

		irq             => via2_irq
	);

-- PSG
 psg_a: psg
  port map (
    clock       => CLK_IN,
    ce          => ENA_1MHZ_N,
    reset       => RESETn,
    bdir        => via1_cb2_out,
    bc1         => via1_ca2_out,
    d           => via1_pa_out,
    q           => psg_do,
    a           => PSG_OUT_A,
    b           => PSG_OUT_B,
    c           => PSG_OUT_C,
    mix         => PSG_OUT,

    ioad        => (OTHERS => 'Z'),
    ioaq        => ym_o_ioa,
    iobd        => (OTHERS => 'Z'),
    iobq        => open,

    sel         => '1'
    );


inst_key : keyboard
	port map(
		clk_sys      => CLK_IN,
		key_pressed  => key_pressed,
		key_extended => key_extended,
		key_strobe   => key_strobe,
		key_code     => key_code,
		row          => via1_pb_out (2 downto 0),
		col          => ym_o_ioa,
		key_hit      => KEY_HIT,
		swnmi        => swnmi,
		swrst        => swrst
);

KEYB_NMIn <= NOT swnmi;
KEYB_RESETn <= NOT swrst;

HCS3119: work.HCS3119 
    port map( 
          CLK_SYS   => CLK_IN,
			 PH2       => ula_PHI2,
			 MAPn      => ula_MAPn,
                                                            -- Oric Expansion Port Signa
			 A         => cpu_ad (15 downto 0),                -- 6502 Address Bus
          RnW       => cpu_rw,                              -- 6502 Read-/Write
			 IO        => ula_CSIOn,
          PA        => via2_pa_out (2 downto 0),            -- VIA1 PA Lines

          CS300n    => CS300n,
			 CS314n    => CS314n,
			 CS318n    => CS318n,
			 CS31Cn    => CS31Cn,
          CS320n    => CS320n,
			 CS360n    => CS360n,
			 CS1793n   => CS1793n,
          CS0n      => CS0n,
          CS1n      => CS1n,
          CS2n      => CS2n,
          CS3n      => CS3n,
          CS4n      => CS4n,
          CS5n      => CS5n,
          CS6n      => CS6n,
 
          WD_CLK    => WD_CLK,
			 WD_REn    => WD_REn,
			 WD_WEn    => WD_WEn,
			 nRESET    => not RESET AND KEYB_RESETn,
			 nHOSTRST  => RESETn                                -- RESET from MCU
         );

HCS3120: work.HCS3120 
    port map( 
          CLK_SYS   => CLK_IN,
			 WD_CLK    => WD_CLK,
																				-- Oric Expansion Port Signals
          DI        => cpu_do,                              -- 6502 Data Bus
			 --FDC_DAL_OUT => FDC_DAL_OUT,
          DO        => CONT_D_OUT,                          -- 6502 Data Bus
          A         => cpu_ad(15 downto 0),                 -- 6502 Address Bus
          RnW       => cpu_rw,                              -- 6502 Read-/Write
          nIRQ      => cont_irq,                            -- 6502 /IRQ
          IO        => ula_CSIOn,                           -- Oric I/O 
 			 CS314n    => CS314n,
			 CS318n    => CS318n,
			 CS1793n   => CS1793n,
          --IOCTRL    => IOCONTn,                             -- Oric I/O Control           
                                                            -- Additional MCU Interface Lines
          nRESET    => RESETn,                              -- RESET from MCU
          
          SS          =>  SSEL,
			 DS0         =>  DS0,
			 DS1         =>  DS1,
			 DS2         =>  DS2,
			 DS3         =>  DS3,
			 WD_IRQ      =>  WD_IRQ,
			 WD_DRQ      =>  WD_DRQ,
			 WD_HLD      =>  WD_HLD,
			 WD_REn      =>  WD_REn
         );
			
			
HCS10017 : entity work.HCS10017
   port map (
      CLK        	=> CLK_IN,
      PHI2       	=> ula_phi2,
		PHI2_EN     => ENA_1MHZ,
		PHI2_EN_N   => ENA_1MHZ_N,
      CLK_4      	=> ula_CLK_4,
		CLK_4_EN    => ula_CLK_4_en,
      RW         	=> cpu_rw,
      RESETn     	=> pll_locked,
		MAPn      	=> ula_MAPn,
      DB         	=> SRAM_DO,
      ADDR       	=> cpu_ad(15 downto 0),
      SRAM_AD    	=> ula_AD_SRAM,
		SRAM_OE    	=> ula_OE_SRAM,
		SRAM_CE    	=> ula_CE_SRAM,
		SRAM_WE    	=> ula_WE_SRAM,
		LATCH_SRAM 	=> ula_LATCH_SRAM,
		CSIOn       => ula_CSIOn,
      CSRAMn     	=> ula_CSRAMn,
      R          	=> VIDEO_R,
      G          	=> VIDEO_G,
      B          	=> VIDEO_B,
		HBLANK      => VIDEO_HBLANK,
		VBLANK      => VIDEO_VBLANK,
		CLK_PIX     => VIDEO_CLK,
		HSYNC      	=> VIDEO_HSYNC,
		VSYNC      	=> VIDEO_VSYNC		
);

			
--HCS10017: oric_ula
--   port map (
--      CLK_24      => CLK_IN,
--      PHI       	=> ula_phi2,
--		PHI_EN     => ENA_1MHZ,
--		PHI_EN_N   => ENA_1MHZ_N,
--      RnW        	=> cpu_rw,
--      nRESET     	=> pll_locked,
--		nMAP      	=> ula_MAPn,
--      D          	=> SRAM_DO,
--      AH       	=> cpu_ad(15 downto 0),
--      RC    	   => ula_AD_SRAM,
--		nRAM    	   => ula_CSRAMn,
--		WE    	   => ula_WE_SRAM,
--		SRAM_CE    	=> ula_CE_SRAM,
--	   nIO         => ula_CSIOn,
--      RED        	=> VIDEO_R,
--      GREEN      	=> VIDEO_G,
--      BLUE        => VIDEO_B,
--		HBLANK      => VIDEO_HBLANK,
--		VBLANK      => VIDEO_VBLANK,
--		HSYNC      	=> VIDEO_HSYNC,
--		VSYNC      	=> VIDEO_VSYNC		
--);


inst_ACIA : work.ACIA
   port map(
          RESET  => RESETn,
			 PHI2   => ENA_1MHZ,
			 CS     => CS31Cn,
			 RWN    => cpu_rw,
			 RS     => cpu_ad(1 downto 0),
			 DATAIN => cpu_do,
			 DATAOUT=> ACIA_DO,
			 XTLI   => CLK_ACIA,
			 RTSB   => RTS, --UART_RTS,
			 CTSB   => RTS, --UART_CTS,
			 DTRB   => DTR, 
			 RXD    => RXD , --UART_RXD,
			 TXD    => RXD , --UART_TXD,
			 IRQn   => acia_irq

);


via1_pa_in <= (via1_pa_out and not via1_pa_out_oe_l) or (psg_do and via1_pa_out_oe_l);
via1_pb_in(2 downto 0) <= via1_pb_out(2 downto 0);
via1_pb_in(3) <= KEY_HIT;
via1_pb_in(4) <=via1_pb_out(4);
via1_pb_in(5) <= 'Z';
via1_pb_in(6) <=via1_pb_out(6);
via1_pb_in(7) <=via1_pb_out(7);

K7_TAPEOUT  <= via1_pb_out(7);
K7_REMOTE   <= via1_pb_out(6);
PRN_STROBE  <= via1_pb_out(4);
PRN_DATA    <= via1_pa_out;



joy_mux <= not joystick_0 when via2_pb_out(7) = '1' else
           not joystick_1 when via2_pb_out(6) = '1' else
			  "11111";
			  
via2_pb_in  <= not via2_pb_out(7) & via2_pb_out(6) & via2_pb_out(5) & joy_mux;
via2_pa_in  <= not fire3_t1 & UART_RXD & not fire2_t1 & via2_pa_out(4) & '1' & via2_pa_out(2 downto 0);

ROM_CSn <= CS6n and cS5n and CS4n and (CS3n or BANK4); 
rom_ad <= not via2_pa_out (1 downto 0) & cpu_ad (13 downto 0);
  

cpu_di <= VIA1_DO          when cs300n = '0' else 
          CONT_D_OUT       when cs314n = '0' else
          CONT_D_OUT       when cs318n = '0' else			 
          FDC_DAL_OUT      when CS1793n = '0' else 
			 ACIA_DO          when CS31Cn = '0' else 
			 VIA2_DO          when CS320n = '0' else
--			 SRAM_DO          when CS360n = '0' else -- placeholder for rtc
          ROM_Q            when ROM_CSn = '0' else
			 RAM_BANK4_DO     when CS3n = '0' else
			 RAM_BANK3_DO     when CS2n = '0' else
			 RAM_BANK2_DO     when CS1n = '0' else
			 RAM_BANK1_DO     when CS0n = '0' else
			 SRAM_DO          when ula_CSRAMn = '0' else
			 CPU_DI_LAST;

process (CLK_IN) begin
	if rising_edge(CLK_IN) then
		if cpu_rw = '1' and ula_phi2 = '1' then
			cpu_di_last <= cpu_di;
		end if;
	end if;
end process;

end RTL;
