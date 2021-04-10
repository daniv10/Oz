-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 -- Tracking a moving object through a camera using FPGA with VHDL --
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_unsigned.all;

entity  gray_scale  is port ( 
	clkIn 						:STD_LOGIC;
	clk_i 						:STD_LOGIC;
	dataBufferIn				:std_logic_vector(11 downto 0);
	dataBufferout				:out std_logic_vector(11 downto 0)
);
end entity;

architecture Behavioral of gray_scale  is

	signal dataBuffer:std_logic_vector(11 downto 0);
	signal red,green,blue:std_logic_vector(7 downto 0);

	--gray scale
	signal mult_r: std_logic_vector(7 downto 0) := x"4D"; -- 0.3 * 256 = 77 = 0x4D
	signal mult_g: std_logic_vector(7 downto 0) := x"97"; -- 0.59 * 256 = 151 = 0x97
	signal mult_b: std_logic_vector(7 downto 0) := x"1C"; 	 -- 0.11 * 256 = 28 = 0x1C

begin
	--inst1hzled: tutorial_led_blink port map(i_clock=>clkIn,o_led_drive=>o_led_drive);
	dataBuffer<=dataBufferIn;
	red	<= dataBuffer(11 downto 8) & "0000";
	green	<= dataBuffer(7 downto 4) & "0000";
	blue	<= dataBuffer(3 downto 0) & "0000";
	
	process(clkIn)
		variable y: std_logic_vector(15 downto 0);
	begin
		if (clk_i'event and clk_i = '1') then									
			y :=  red * mult_r + green * mult_g + blue * mult_b;
			dataBufferout <= y(15 downto 12) & y(15 downto 12) & y(15 downto 12);				
		end if;
	end process;
end Behavioral;

LIBRARY IEEE;
USE  IEEE.STD_LOGIC_1164.all;
USE  IEEE.STD_LOGIC_ARITH.all;
USE  IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.NUMERIC_STD.ALL;


ENTITY LCD_DISPLAY_nty IS
   
   PORT( 
      reset              : IN     std_logic;  -- Map this Port to a Switch within your [Port Declarations / Pin Planner]  
      clock_50           : IN     std_logic;  -- The DE2 50Mhz Clk and the "clk_count_400hz" counter variable are used to Genreate a 400Hz clock pulse 
                                              -- to drive the LCD CORE state machine.
      
      lcd_rs             : OUT    std_logic;
      lcd_e              : OUT    std_logic;
      lcd_rw             : OUT    std_logic;
      lcd_on             : OUT    std_logic;
      lcd_blon           : OUT    std_logic;
      
      data_bus_0         : INOUT  STD_LOGIC;
      data_bus_1         : INOUT  STD_LOGIC;
      data_bus_2         : INOUT  STD_LOGIC;
      data_bus_3         : INOUT  STD_LOGIC;
      data_bus_4         : INOUT  STD_LOGIC;
      data_bus_5         : INOUT  STD_LOGIC;
      data_bus_6         : INOUT  STD_LOGIC;
      data_bus_7         : INOUT  STD_LOGIC;
		
      number_to_display1: in integer;
		number_to_display2: in integer;
		number_to_display3: in integer;
		number_to_display4: in integer;
		
		sw2_take_snapshot1 		: in std_logic;
		sw3_take_snapshot2		: in std_logic;
		sw4_display_snapshot1	: in std_logic;
		sw5_display_snapshot2	: in std_logic;
		sw6_auto_enable			: in std_logic;
		sw7_sub_display			: in std_logic;
		sw8_sub_activate			: in std_logic;
		sw9_thresh_activate		: in std_logic;
		sw10_thresh_display		: in std_logic;
		sw11_median_activate	: in std_logic;
		sw12_median_display		: in std_logic;
		sw13_all_activate		: in std_logic;
		sw14_all_disp 			: in std_logic;
		sw15_location_disp 		: in std_logic;
      LCD_CHAR_ARRAY_3    : IN    STD_LOGIC
     
      
      
   );

           

END LCD_DISPLAY_nty ;

--
ARCHITECTURE LCD_DISPLAY_arch OF LCD_DISPLAY_nty IS
  type character_string is array ( 0 to 31 ) of STD_LOGIC_VECTOR( 7 downto 0 );
  --type character_string2 is array ( 0 to 31 ) of STD_LOGIC_VECTOR( 7 downto 0 );
  
  type state_type is (func_set, display_on, mode_set, print_string,
                      line2, return_home, drop_lcd_e, reset1, reset2,
                       reset3, display_off, display_clear );
                       
  signal state, next_command         : state_type;
  
  
  signal lcd_display_string          : character_string;
  
  signal lcd_display_string_00       : character_string;
  signal lcd_display_string_01       : character_string;
  signal lcd_display_string_02       : character_string;
  signal lcd_display_string_03       : character_string;
  signal lcd_display_string_04       : character_string;
  signal lcd_display_string_05       : character_string;
  signal lcd_display_string_06       : character_string;
  signal lcd_display_string_07       : character_string;
  signal lcd_display_string_08       : character_string;
  signal lcd_display_string_09       : character_string;
  signal lcd_display_string_10       : character_string;
  signal lcd_display_string_11       : character_string;
  signal lcd_display_string_12       : character_string;
  signal lcd_display_string_13       : character_string;
  signal lcd_display_string_14       : character_string;
  signal lcd_display_string_15       : character_string;
  signal lcd_display_string_17       : character_string;
  signal lcd_display_string_numbers  : character_string;
  
  signal data_bus_value, next_char   : STD_LOGIC_VECTOR(7 downto 0);
  signal clk_count_400hz             : STD_LOGIC_VECTOR(23 downto 0);
  signal char_count                  : STD_LOGIC_VECTOR(4 downto 0);
  signal clk_400hz_enable,lcd_rw_int : std_logic;
 -- signal number								 : integer;
  signal Hex_Display_Data            : STD_LOGIC_VECTOR(7 DOWNTO 0); 
  signal data_bus                    : STD_LOGIC_VECTOR(7 downto 0);	
 -- signal LCD_CHAR_ARRAY              : STD_LOGIC_VECTOR(3 DOWNTO 0);
  signal number:integer:=24649;
  signal tempNumberA,tempNumber1A,tempNumber2A,tempNumber3A,tempNumber4A,tempNumber5A,tempNumber6A:integer;
  signal tempNumberB,tempNumber1B,tempNumber2B,tempNumber3B,tempNumber4B,tempNumber5B,tempNumber6B:integer;
  signal tempNumberC,tempNumber1C,tempNumber2C,tempNumber3C,tempNumber4C,tempNumber5C,tempNumber6C:integer;
  signal tempNumberD,tempNumber1D,tempNumber2D,tempNumber3D,tempNumber4D,tempNumber5D,tempNumber6D:integer;

BEGIN
  


--===================================================--  
-- SIGNAL STD_LOGIC_VECTORS assigned to OUTPUT PORTS 
--===================================================--    
--Hex_Display_Data(0) <= Hex_Display_Data_0;
--Hex_Display_Data(1) <= Hex_Display_Data_1;   
--Hex_Display_Data(2) <= Hex_Display_Data_2;
--Hex_Display_Data(3) <= Hex_Display_Data_3;  
--Hex_Display_Data(4) <= Hex_Display_Data_4;
--Hex_Display_Data(5) <= Hex_Display_Data_5;  
--Hex_Display_Data(6) <= Hex_Display_Data_6;
--Hex_Display_Data(7) <= Hex_Display_Data_7;  

data_bus_0 <= data_bus(0);
data_bus_1 <= data_bus(1);
data_bus_2 <= data_bus(2);
data_bus_3 <= data_bus(3);
data_bus_4 <= data_bus(4);
data_bus_5 <= data_bus(5);
data_bus_6 <= data_bus(6);
data_bus_7 <= data_bus(7);


--number<=number_to_display;
    
--LCD_CHAR_ARRAY(0) <= LCD_CHAR_ARRAY_0;
--LCD_CHAR_ARRAY(1) <= LCD_CHAR_ARRAY_1;
--LCD_CHAR_ARRAY(2) <= LCD_CHAR_ARRAY_2;
--LCD_CHAR_ARRAY(3) <= LCD_CHAR_ARRAY_3;
--=====================================--


  

--===============================-- 
--  HD44780 CHAR DATA HEX VALUES --
--===============================-- 
--   = x"20",
-- ! = x"21",
-- " = x"22",
-- # = x"23",
-- $ = x"24",
-- % = x"25",
-- & = x"26",
-- ' = x"27",
-- ( = x"28",
-- ) = x"29",
-- * = x"2A",
-- + = x"2B",
-- , = x"2C",
-- - = x"2D",
-- . = x"2E",
-- / = x"2F",



-- 0 = x"30",
-- 1 = x"31",
-- 2 = x"32",
-- 3 = x"33",
-- 4 = x"34",
-- 5 = x"35",
-- 6 = x"36",
-- 7 = x"37",
-- 8 = x"38",
-- 9 = x"39",
-- : = x"3A",
-- ; = x"3B",
-- < = x"3C",
-- = = x"3D",
-- > = x"3E",
-- ? = x"3F",




-- Q = x"40",
-- A = x"41",
-- B = x"42",
-- C = x"43",
-- D = x"44",
-- E = x"45",
-- F = x"46",
-- G = x"47",
-- H = x"48",
-- I = x"49",
-- J = x"4A",
-- K = x"4B",
-- L = x"4C",
-- M = x"4D",
-- N = x"4E",
-- O = x"4F",

--x"4F", x"5A",x"20",x"44",x"41",x"4E",x"49",x"56",
--x"52", x"41",x"4D",x"20",x"53",x"4F",x"46",x"46",x"45",x"52",

-- P = x"50",
-- Q = x"51",
-- R = x"52",
-- S = x"53",
-- T = x"54",
-- U = x"55",
-- V = x"56",
-- W = x"57",
-- X = x"58",
-- Y = x"59",
-- Z = x"5A",
-- [ = x"5B",
-- Y! = x"5C",
-- ] = x"5D",
-- ^ = x"5E",
-- _ = x"5F",



-- \ = x"60",
-- a = x"61",
-- b = x"62",
-- c = x"63",
-- d = x"64",
-- e = x"65",
-- f = x"66",
-- g = x"67",
-- h = x"68",
-- i = x"69",
-- j = x"6A",
-- k = x"6B",
-- l = x"6C",
-- m = x"6D",
-- n = x"6E",
-- o = x"6F",



-- p = x"70",
-- q = x"71",
-- r = x"72",
-- s = x"73",
-- t = x"74",
-- u = x"75",
-- v = x"76",
-- w = x"77",
-- x = x"78",
-- y = x"79",
-- z = x"7A",
-- { = x"7B",
-- | = x"7C",
-- } = x"7D",
-- -> = x"7E",
-- <- = x"7F",


   lcd_display_string_00 <= 
        (
-- Line 1                          A     c     t     i      v     a     t    e   
         x"20",x"20",x"20",x"20",x"41",x"63",x"74",x"69",x"76",x"61",x"74",x"65",x"20",x"20",x"20",x"20",
   
-- Line 2   ->    				 a    u     t     o                  m     o     d     e
          x"20",x"20",x"20",x"61",x"75",x"74",x"6F",x"20" ,x"20",x"6D",x"6F",x"64",x"65",x"20",x"20",x"20"
   );
   

 lcd_display_string_02 <= 
 (
-- Line 1   											    T    a     k     e          
          x"20",x"20",x"20",x"20",x"20",x"20",x"54",x"61",x"6B",x"65",x"20",x"20",x"20",x"20",x"20",x"20",
   
-- Line 2   ->						 s     n     a     p     s     h      o     t          1
          x"20",x"20",x"20",x"73",x"6E",x"61",x"70",x"73",x"68",x"6F",x"74",x"20",x"31",x"20",x"20",x"20"
   );
   
   
   
   
   
   
   
   
--=====================================================================================================================
   lcd_display_string_03 <= 
  (
-- Line 1   											    T    a     k     e          
          x"20",x"20",x"20",x"20",x"20",x"20",x"54",x"61",x"6B",x"65",x"20",x"20",x"20",x"20",x"20",x"20",
   
-- Line 2   ->						 s     n     a     p     s     h      o     t          2
          x"20",x"20",x"20",x"73",x"6E",x"61",x"70",x"73",x"68",x"6F",x"74",x"20",x"32",x"20",x"20",x"20"
   );
   




--=====================================================================================================================  
   lcd_display_string_04 <= 
    (
-- Line 1                          D     i     s     p      l     a     y       
         x"20",x"20",x"20",x"20",x"44",x"69",x"73",x"70",x"6c",x"61",x"79",x"20",x"20",x"20",x"20",x"20",
   
-- Line 2   ->                 s     n     a     p     s     h      o     t          1
          x"20",x"20",x"20",x"73",x"6E",x"61",x"70",x"73",x"68",x"6F",x"74",x"20",x"31",x"20",x"20",x"20" 
   );
   




--====================================================================================================================== 
   lcd_display_string_05 <= 
    (
-- Line 1                          D     i     s     p      l     a     y       
         x"20",x"20",x"20",x"20",x"44",x"69",x"73",x"70",x"6c",x"61",x"79",x"20",x"20",x"20",x"20",x"20",
   
-- Line 2   ->                 s     n     a     p     s     h      o     t          2
          x"20",x"20",x"20",x"73",x"6E",x"61",x"70",x"73",x"68",x"6F",x"74",x"20",x"32",x"20",x"20",x"20" 
   );
   
   
   
   
   






--====================================================================================================================== 
   lcd_display_string_06 <= 
        (
-- Line 1                                 E     n     a    b     l      e
          x"20",x"20",x"20",x"20",x"20",x"45",x"6E",x"61",x"62",x"6c",x"65",x"20",x"20",x"20",x"20",x"20",
   
-- Line 2   ->    				 a    u     t     o                  m     o     d     e
          x"20",x"20",x"20",x"61",x"75",x"74",x"6F",x"20" ,x"20",x"6D",x"6F",x"64",x"65",x"20",x"20",x"20"
   );
   
   
    
   






--=======================================================================================================================  
   lcd_display_string_07 <= 
    (
-- Line 1                          D     i     s     p      l     a     y       
         x"20",x"20",x"20",x"20",x"44",x"69",x"73",x"70",x"6c",x"61",x"79",x"20",x"20",x"20",x"20",x"20",
   
-- Line 2   ->                 s     u     b     t     r     a     c     t     i     o    n
          x"20",x"20",x"20",x"73",x"75",x"62",x"74",x"72",x"61",x"63",x"74",x"69",x"6F",x"6E",x"20",x"20" 
   );
   
   
   
   






--======================================================================================================================== 
   lcd_display_string_08 <= 
    (
-- Line 1                          A     c     t     i      v     a     t    e   
         x"20",x"20",x"20",x"20",x"41",x"63",x"74",x"69",x"76",x"61",x"74",x"65",x"20",x"20",x"20",x"20",
   
-- Line 2   ->                 s     u     b     t     r     a     c     t     i     o    n
          x"20",x"20",x"20",x"73",x"75",x"62",x"74",x"72",x"61",x"63",x"74",x"69",x"6F",x"6E",x"20",x"20" 
   );
   
   
   
   





--=========================================================================================================================   
   lcd_display_string_09 <= 
    (
-- Line 1                          A     c     t     i      v     a     t    e   
         x"20",x"20",x"20",x"20",x"41",x"63",x"74",x"69",x"76",x"61",x"74",x"65",x"20",x"20",x"20",x"20",
			
-- Line 2   ->                  		t      h     r     e    s     h     o     l     d
          x"20",x"20",x"20",x"20",x"74",x"68",x"72",x"65",x"73",x"68",x"6F",x"6c",x"64",x"20",x"20",x"20" 
   );
   
   
   



--==========================================================================================================================   
   lcd_display_string_10 <= 
	(
-- Line 1                          D     i     s     p      l     a     y       
         x"20",x"20",x"20",x"20",x"44",x"69",x"73",x"70",x"6c",x"61",x"79",x"20",x"20",x"20",x"20",x"20",
			
-- Line 2   ->                  		t      h     r     e    s     h     o     l     d
          x"20",x"20",x"20",x"20",x"74",x"68",x"72",x"65",x"73",x"68",x"6F",x"6c",x"64",x"20",x"20",x"20" 
   );
	
	
--==========================================================================================================================   
   lcd_display_string_11 <= 
	(
-- Line 1                          A     c     t     i      v     a     t    e   
         x"20",x"20",x"20",x"20",x"41",x"63",x"74",x"69",x"76",x"61",x"74",x"65",x"20",x"20",x"20",x"20",
			
-- Line 2   ->    m     e      d    i     a      n                f     i     l     t     e     r
          x"20",x"6D",x"65",x"64",x"69",x"61",x"6E",x"20",x"20",x"66",x"69",x"6c",x"74",x"65",x"72",x"20" 
   );
	
	--==========================================================================================================================   
   lcd_display_string_12 <= 
	(
-- Line 1                          D     i     s     p      l     a     y       
         x"20",x"20",x"20",x"20",x"44",x"69",x"73",x"70",x"6c",x"61",x"79",x"20",x"20",x"20",x"20",x"20",
				
-- Line 2   ->    m     e      d    i     a      n                f     i     l     t     e     r
          x"20",x"6D",x"65",x"64",x"69",x"61",x"6E",x"20",x"20",x"66",x"69",x"6c",x"74",x"65",x"72",x"20" 
   );
   
		--==========================================================================================================================   
   lcd_display_string_13 <= 
	(
-- Line 1                                 E     n     a    b     l      e
          x"20",x"20",x"20",x"20",x"20",x"45",x"6E",x"61",x"62",x"6c",x"65",x"20",x"20",x"20",x"20",x"20",
   
-- Line 2   a      u    t     o                  p     r     o     c     e     s     s     i     n     g
          x"61",x"75",x"74",x"6F",x"20",x"20",x"70",x"72" ,x"6F",x"63",x"65",x"73",x"73",x"69",x"6E",x"67"
   );
   
   

--===========================================================================================================================  
   lcd_display_string_15 <= 
    (
-- Line 1                          D     i     s     p      l     a     y       
         x"20",x"20",x"20",x"20",x"44",x"69",x"73",x"70",x"6c",x"61",x"79",x"20",x"20",x"20",x"20",x"20",
				
-- Line 2                            l     o    c     a     t    i      o     n
          x"20",x"20",x"20",x"20",x"6c",x"6F",x"63",x"61",x"74",x"69",x"6F",x"6E",x"20",x"20",x"20",x"20"
   );
   
--===========================================================================================================================  
  
	lcd_display_string_17 <= 
    (             
	-- O      Z            D    A     N     I     V
	x"20",x"20",x"20",x"20", x"4F", x"5A",x"20",x"44",x"41",x"4E",x"49",x"56", x"20",x"20",x"20",x"20",
	--					     R		A		M				      S		O		F		E		R
	x"20",x"20",x"20",x"52", x"41", x"4D",x"20",x"20",x"53",x"4F",x"46",x"45",x"52",x"20",x"20",x"20"
   );
--===========================================================================================================================  
  
   lcd_display_string_numbers <= 
    (
          x"30",x"31",x"32",x"33",x"34",x"35",x"36",x"37",x"38",x"39",x"76",x"65",x"65",x"54",x"48",x"54",
   
-- Line 2   ->     %    1     0      0
          x"7E",x"25",x"31",x"30",x"30",x"42",x"4C",x"55",x"45",x"54",x"4F",x"4F",x"54",x"48",x"54",x"20"
   );
	



-------------------------------------------------------------------------------------------------------
-- BIDIRECTIONAL TRI STATE LCD DATA BUS
   data_bus <= data_bus_value when lcd_rw_int = '0' else "ZZZZZZZZ";
   
-- LCD_RW PORT is assigned to it matching SIGNAL 
 lcd_rw <= lcd_rw_int;
 
 
 
 
 
 

------------------------------------ STATE MACHINE FOR LCD SCREEN MESSAGE SELECT -----------------------------
---------------------------------------------------------------------------------------------------------------
PROCESS (LCD_CHAR_ARRAY_3,	sw2_take_snapshot1, sw3_take_snapshot2, sw4_display_snapshot1, sw5_display_snapshot2, sw6_auto_enable, sw7_sub_display, sw8_sub_activate,	sw9_thresh_activate,	sw10_thresh_display,	sw11_median_activate, sw12_median_display, sw13_all_activate, sw14_all_disp, sw15_location_disp)
 -- variable number:integer:=24649;
 -- variable tempNumber1,tempNumber2,tempNumber3,tempNumber4,tempNumber5:integer;
--	variable tempNumber:integer;
BEGIN
  
-- get next character in display string based on the LCD_CHAR_ARRAY (switches or Multiplexer)
	 if char_count="00000" then
			tempNumberA<=number_to_display1;
			tempNumberB<=number_to_display2;
			tempNumberC<=number_to_display3;
			tempNumberD<=number_to_display4;
	 end if;
	 if LCD_CHAR_ARRAY_3 = '1' then
		 next_char <= lcd_display_string_17(CONV_INTEGER(char_count));  
		 if sw6_auto_enable ='1' and sw13_all_activate ='1' then
			next_char <= lcd_display_string_00(CONV_INTEGER(char_count));
		 elsif sw2_take_snapshot1 ='1' then
			next_char <= lcd_display_string_02(CONV_INTEGER(char_count));
		 elsif sw3_take_snapshot2 ='1' then
			next_char <= lcd_display_string_03(CONV_INTEGER(char_count));
		 elsif sw4_display_snapshot1 ='1' then
			next_char <= lcd_display_string_04(CONV_INTEGER(char_count));
		 elsif sw5_display_snapshot2 ='1' then
			next_char <= lcd_display_string_05(CONV_INTEGER(char_count));
		 elsif sw6_auto_enable ='1' then
			next_char <= lcd_display_string_06(CONV_INTEGER(char_count));
		 elsif sw7_sub_display ='1' then
			next_char <= lcd_display_string_07(CONV_INTEGER(char_count));
		 elsif sw8_sub_activate ='1' then
			next_char <= lcd_display_string_08(CONV_INTEGER(char_count));
		 elsif sw9_thresh_activate ='1' then
			next_char <= lcd_display_string_09(CONV_INTEGER(char_count));
		 elsif sw10_thresh_display ='1' then
			next_char <= lcd_display_string_10(CONV_INTEGER(char_count));
		 elsif sw11_median_activate ='1' then
			next_char <= lcd_display_string_11(CONV_INTEGER(char_count));
		 elsif sw12_median_display ='1' then
			next_char <= lcd_display_string_12(CONV_INTEGER(char_count));
		 elsif sw13_all_activate ='1' then
			next_char <= lcd_display_string_13(CONV_INTEGER(char_count));
		-- elsif sw14_all_disp ='1' then
		--	next_char <= lcd_display_string_14(CONV_INTEGER(char_count));	
		 elsif sw15_location_disp = '1' then
			next_char <= lcd_display_string_15(CONV_INTEGER(char_count)); 
		 end if;
    else
	 
							tempNumber1A<=tempNumberA mod 10;					
							tempNumber2A<=(tempNumberA mod 100) / 10;
							tempNumber3A<=(tempNumberA mod 1000) / 100;
							tempNumber4A<=(tempNumberA mod 10000) / 1000;
							tempNumber5A<=(tempNumberA mod 100000) / 10000;
							tempNumber6A<=tempNumberA / 100000;
							
							tempNumber1B<=tempNumberB mod 10;					
							tempNumber2B<=(tempNumberB mod 100) / 10;
							tempNumber3B<=(tempNumberB mod 1000) / 100;
							tempNumber4B<=(tempNumberB mod 10000) / 1000;
							tempNumber5B<=(tempNumberB mod 100000) / 10000;
							tempNumber6B<=tempNumberB / 100000;
							
							tempNumber1C<=tempNumberC mod 10;					
							tempNumber2C<=(tempNumberC mod 100) / 10;
							tempNumber3C<=(tempNumberC mod 1000) / 100;
							tempNumber4C<=(tempNumberC mod 10000) / 1000;
							tempNumber5C<=(tempNumberC mod 100000) / 10000;
							tempNumber6C<=tempNumberC / 100000;
							
							tempNumber1D<=tempNumberD mod 10;					
							tempNumber2D<=(tempNumberD mod 100) / 10;
							tempNumber3D<=(tempNumberD mod 1000) / 100;
							tempNumber4D<=(tempNumberD mod 10000) / 1000;
							tempNumber5D<=(tempNumberD mod 100000) / 10000;
							tempNumber6D<=tempNumberD / 100000;
					
					if char_count=0 then
						if tempNumberA>=100000 then
							next_char <= lcd_display_string_numbers(tempNumber6A);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=1 then
						if tempNumberA>=10000 then
							next_char <= lcd_display_string_numbers(tempNumber5A);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=2 then
						if tempNumberA>=1000 then
							next_char <= lcd_display_string_numbers(tempNumber4A);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=3 then
						if tempNumberA>=100 then
							next_char <= lcd_display_string_numbers(tempNumber3A);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=4 then
						if tempNumberA>=10 then
							next_char <= lcd_display_string_numbers(tempNumber2A);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=5 then
						next_char <= lcd_display_string_numbers(tempNumber1A);
					end if;
					if char_count=10 then
						if tempNumberC>=100000 then
							next_char <= lcd_display_string_numbers(tempNumber6C);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=11 then
						if tempNumberC>=10000 then
							next_char <= lcd_display_string_numbers(tempNumber5C);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=12 then
						if tempNumberC>=1000 then
							next_char <= lcd_display_string_numbers(tempNumber4C);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=13 then
						if tempNumberC>=100 then
							next_char <= lcd_display_string_numbers(tempNumber3C);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=14 then
						if tempNumberC>=10 then
							next_char <= lcd_display_string_numbers(tempNumber2C);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=15 then
						next_char <= lcd_display_string_numbers(tempNumber1C);
					end if;
					if char_count=16 then
						if tempNumberB>=100000 then
							next_char <= lcd_display_string_numbers(tempNumber6B);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=17 then
						if tempNumberB>=10000 then
							next_char <= lcd_display_string_numbers(tempNumber5B);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=18 then
						if tempNumberB>=1000 then
							next_char <= lcd_display_string_numbers(tempNumber4B);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=19 then
						if tempNumberB>=100 then
							next_char <= lcd_display_string_numbers(tempNumber3B);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=20 then
						if tempNumberB>=10 then
							next_char <= lcd_display_string_numbers(tempNumber2B);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=21 then
						next_char <= lcd_display_string_numbers(tempNumber1B);
					end if;
					if char_count=26 then
						if tempNumberD>=100000 then
							next_char <= lcd_display_string_numbers(tempNumber6D);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=27 then
						if tempNumberD>=10000 then
							next_char <= lcd_display_string_numbers(tempNumber5D);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=28 then
						if tempNumberD>=1000 then
							next_char <= lcd_display_string_numbers(tempNumber4D);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=29 then
						if tempNumberD>=100 then
							next_char <= lcd_display_string_numbers(tempNumber3D);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=30 then
						if tempNumberD>=10 then
							next_char <= lcd_display_string_numbers(tempNumber2D);
						else
							next_char <= lcd_display_string_numbers(31);
						end if;
					end if;
					if char_count=31 then
						next_char <= lcd_display_string_numbers(tempNumber1D);
					end if;
					if (char_count>5 and char_count<10) or (char_count>21 and char_count<26) then
						next_char <= lcd_display_string_numbers(31);
					end if;	
					


					
				--	else
				--	if tempNumber/=0 then
						
				--	else
					--	next_char <= lcd_display_string_numbers(tempDivNumber);
           --   end if;                                       
       end if;
END PROCESS;
   

 
  
  
  
--======================= CLOCK SIGNALS ============================--  
process(clock_50)
begin
      if (rising_edge(clock_50)) then
         if (reset = '0') then
            clk_count_400hz <= x"000000";
            clk_400hz_enable <= '0';
         else
           
           
--== NOTE for the IF statement below:
----------------------------------------------------------------------------------------------------------------------------------------------------------------------- 
-- Due to the fact that each STATE in the LCD Driver State Machine... (Shown further below) ...each state will immediately be followed by the "drop_lcd_e" STATE....
-- this will cause the Period of the "lcd_e" signal to be doubled when viewed on an Oscilloscope. This also causes your set frequency value 
-- to be divided in half. The variable "clk_count_400hz" whichever hex value you choose for a specific Frequency, that frequency will be divided in half.  
-- i.e: (clk_count_400hz <= x"7A120") is the value for a 100hz signal, however when you monitor the LCD's "ENABLE" port with an oscilloscope; it will be detected as 
-- a 50Hz signal. (Half the Set Frequency). This is becasue the State machine cycles twice!!! Creating 1 Full Cycle for the "LCD_E" Port. Logic HI...then....LOGIC LOW.
--======================================================================================================================================================================             
           
          if (clk_count_400hz <= x"00F424") then            
                                                 -- If using the DE2 50Mhz Clock,  use clk_count_1600hz <= x"007A12"  (50Mhz/1600hz = 32250   converted to HEX = 7A12 )
                                                 --                                use clk_count_1250hz <= x"009C40"  (50Mhz/1250hz = 40000   converted to HEX = 9C40 )
                                                 --                                use clk_count_800hz  <= x"00F424"  (50Mhz/800hz  = 62500   converted to HEX = F424 )
                                                 --                                use clk_count_400hz  <= x"01E848"  (50Mhz/400hz  = 125000  converted to HEX = 1E848 )
                                                 --                                use clk_count_200hz  <= x"03D090"  (50Mhz/200hz  = 250000  converted to HEX = 3D090 )
                                                 --                                use clk_count_100hz  <= x"07A120"  (50Mhz/100hz  = 500000  converted to HEX = 7A120 )
                                                 --                                use clk_count_50hz   <= x"0F4240"  (50Mhz/50hz   = 1000000 converted to HEX = F4240 )
                                                 --                                use clk_count_10hz   <= x"4C4B40"  (50Mhz/10hz   = 5000000 converted to HEX = 4C4B40 )
                                                           
                                                 --  In Theory for a 27Mhz Clock,  use clk_count_400hz <= x"107AC"  (27Mhz/400hz = 67500  converted to HEX = 107AC )
                                                 --                                use clk_count_200hz <= x"20F58"  (27Mhz/200hz = 125000 converted to HEX = 20F58 )
                                                             
                                                 --  In Theory for a 25Mhz Clock.  use clk_count_400hz <= x"00F424"  (25Mhz/400hz = 62500  converted to HEX = F424 )
                                                 --                                use clk_count_200hz <= x"01E848"  (25Mhz/200hz = 125000 converted to HEX = 1E848 )
                                                 --                                use clk_count_100hz <= x"03D090"  (25Mhz/100hz = 250000 converted to HEX = 3D090 )
                                                           
                                                           
                   clk_count_400hz <= clk_count_400hz + 1;                                          
                   clk_400hz_enable <= '0';                
           else
                   clk_count_400hz <= x"000000";
                   clk_400hz_enable <= '1';
            end if;
         end if;
      end if;
end process;  
--==================================================================--    
  
  
  
  
--======================== LCD DRIVER CORE ==============================--   
--                     STATE MACHINE WITH reset                          -- 
--===================================================-----===============--  
process (clock_50, reset)
begin
        if reset = '0' then
           state <= reset1;
           data_bus_value <= x"38"; -- reset
           next_command <= reset2;
           lcd_e <= '1';
           lcd_rs <= '0';
           lcd_rw_int <= '0';  
    
    
    
        elsif rising_edge(clock_50) then
             if clk_400hz_enable = '1' then  
                 
                 
                 
              --========================================================--                 
              -- State Machine to send commands and data to LCD DISPLAY
              --========================================================--
                 case state is
                 -- Set Function to 8-bit transfer and 2 line display with 5x8 Font size
                 -- see Hitachi HD44780 family data sheet for LCD command and timing details
                       
                       
                       
--======================= INITIALIZATION START ============================--
                       when reset1 =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"38"; -- EXTERNAL reset
                            state <= drop_lcd_e;
                            next_command <= reset2;
                            char_count <= "00000";
  
                       when reset2 =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"38"; -- EXTERNAL reset
                            state <= drop_lcd_e;
                            next_command <= reset3;
                            
                       when reset3 =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"38"; -- EXTERNAL reset
                            state <= drop_lcd_e;
                            next_command <= func_set;
            
            
                       -- Function Set
                       --==============--
                       when func_set =>                
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"38";  -- Set Function to 8-bit transfer, 2 line display and a 5x8 Font size
                            state <= drop_lcd_e;
                            next_command <= display_off;
                            
                            
                            
                       -- Turn off Display
                       --==============-- 
                       when display_off =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"08"; -- Turns OFF the Display, Cursor OFF and Blinking Cursor Position OFF.......
                                                     -- (0F = Display ON and Cursor ON, Blinking cursor position ON)
                            state <= drop_lcd_e;
                            next_command <= display_clear;
                           
                           
                       -- Clear Display 
                       --==============--
                       when display_clear =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"01"; -- Clears the Display    
                            state <= drop_lcd_e;
                            next_command <= display_on;
                           
                           
                           
                       -- Turn on Display and Turn off cursor
                       --===================================--
                       when display_on =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"0C"; -- Turns on the Display (0E = Display ON, Cursor ON and Blinking cursor OFF) 
                            state <= drop_lcd_e;
                            next_command <= mode_set;
                          
                          
                       -- Set write mode to auto increment address and move cursor to the right
                       --====================================================================--
                       when mode_set =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"06"; -- Auto increment address and move cursor to the right
                            state <= drop_lcd_e;
                            next_command <= print_string; 
                            
                                
--======================= INITIALIZATION END ============================--                          
                          
                          
                          
                          
--=======================================================================--                           
--               Write ASCII hex character Data to the LCD
--=======================================================================--
                       when print_string =>          
                            state <= drop_lcd_e;
                            lcd_e <= '1';
                            lcd_rs <= '1';
                            lcd_rw_int <= '0';
                          
                          
                               -- ASCII character to output
                               -----------------------------
                               -- Below we check to see if the Upper-Byte of the HEX number being displayed is x"0"....We use this number x"0" as a Control Variable, 
                               -- to know when a certain condition is met.  Next, we proceed to process the "next_char" variable to Sequentially count up in HEX format.
                               
                               -- This is required because as you know...Counting in HEX...after #9 comes Letter A.... well if you look at the ASCII CHART, 
                               -- the Letters A,B,C etc. are in a different COLUMN compared to the one the Decimal Numbers are in.  Letters A...F are in Column  x"4".    
                               -- Numbers 0...9 are in Column x"3".  The Upper-Byte controls which COLUMN the Character will be selected from... and then Displayed on the LCD. 
                               
                               -- So to Count up seamlessly using our 4-Bit Variable 8,9,10,11 and so on...we need to set some IF THEN ELSE conditions 
                               -- to control this changing of Columns so that it will be displayed counting up in HEX Format....8,9,A,B,C,D etc.
                                
                               -- Also, if the High-Byte is detected as an actual Character Column from the ASCII CHART that has Valid Characters 
                               -- (Like using a Upper-Byte of x"2",x"3",x"4",x"5",x"6" or x"7") then it will just go ahead and decalre  "data_bus_value <= next_char;"  
                               -- and the "Print_Sring" sequence will continue to execute. These HEX Counting conditions are only being applied to the Variables that have 
                               -- the x"0" Upper-Byte value.....For our code that is the:  [x"0"&hex_display_data]  variable.  
                               
                               
                               if (next_char(7 downto 4) /= x"0") then
                                  data_bus_value <= next_char;
                               else
                             
                                    -- Below we process a 4-bit STD_LOGIC_VECTOR that is counting up Sequentially, we process the values so that it Displays in HEX Format as it counts up.
                                    -- In our case, our SWITCHES have been Mapped to a 4-bit STD_LOGIC_VECTOR and we have placed an Upper-Byte value of x"0" before it.
                                    -- This triggers the Process below, which will condition which numbers and letters are displayed on the LCD as the 4-Bit Variable counts up past #9 or 1001
                                    -------------------------------------------------------------------------------------------------------------------------------------------------------------
                                    
                                    -- if the number is Greater than 9..... meaning the number is now in the Realm of HEX... A,B,C,D,E,F... then
                                    if next_char(3 downto 0) >9 then 
                              
                                    -- See the ASCII CHART... Letters A...F are in Column  x"4"
                                      data_bus_value <= x"4" & (next_char(3 downto 0)-9);  
                                    else 
                                
                                    -- See the ASCII CHART... Numbers 0...9 are in Column x"3"
                                      data_bus_value <= x"3" & next_char(3 downto 0);
                                    end if;
                               end if;
                          
                          
                          

                            -- Loop to send out 32 characters to LCD Display (16 by 2 lines)
                               if (char_count < 31) AND (next_char /= x"fe") then
												char_count <= char_count +1;     
                               else
                                   char_count <= "00000";
                               end if;
                  
                  
                  
                            -- Jump to second line?
                               if char_count = 15 then 
                                  next_command <= line2;
                   
                   
                   
                            -- Return to fireset line?
                               elsif (char_count = 31) or (next_char = x"fe") then
                                     next_command <= return_home;
                               else 
                                     next_command <= print_string; 
                               end if; 
                 
                 
                 
                       -- Set write address to line 2 character 1
                       --======================================--
                       when line2 =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"c0";
                            state <= drop_lcd_e;
                            next_command <= print_string;      
                     
                     
                       -- Return write address to fireset character position on line 1
                       --=========================================================--
                       when return_home =>
                            lcd_e <= '1';
                            lcd_rs <= '0';
                            lcd_rw_int <= '0';
                            data_bus_value <= x"80";
                            state <= drop_lcd_e;
                            next_command <= print_string; 
                    
                    
                    
                       -- The next states occur at the end of each command or data transfer to the LCD
                       -- Drop LCD E line - falling edge loads inst/data to LCD controller
                       --============================================================================--
                       when drop_lcd_e =>
                            state <= next_command;
                            lcd_e <= '0';
                            lcd_blon <= '1';
                            lcd_on   <= '1';
                        end case;




             end if;-- CLOSING STATEMENT FOR "IF clk_400hz_enable = '1' THEN"
             
      end if;-- CLOSING STATEMENT FOR "IF reset = '0' THEN" 
      
end process;                                                            
  
END ARCHITECTURE LCD_DISPLAY_arch;
--clk divider from 50mhz to 1 mhz

library ieee;
use ieee.std_logic_1164.all; 
use ieee.std_logic_unsigned.all;

entity pwmg is
	generic(clk_divide : integer := 25);
	port(
		reset_n : in std_logic;
		clk_in  : in std_logic;	
		clk_out : out std_logic
	);
end pwmg;


architecture behav of pwmg is
	signal clk_cnt: integer:= 0 ;
	signal temp: std_logic:= '0' ;
	

begin 	
	
	process(clk_in, reset_n)
		begin
		if (reset_n = '1') then
			clk_cnt <= 0;
			temp 	<= '0';
		elsif (rising_edge(clk_in)) then
			if(clk_cnt = clk_divide -1 ) then
				clk_cnt <= 0 ;
			temp	<= not temp;
			else
				clk_cnt <= clk_cnt + 1;
			end if;
		end if;
	end process;
	
	clk_out <= temp;

end behav;



--servo commands for shift angles


library ieee;
use ieee.std_logic_1164.all; 
use IEEE.STD_LOGIC_ARITH.ALL;
use ieee.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.ALL;
	

entity pwmg_servo is 
		generic( clk_divide : integer := 20000;
					duty1   	  : integer := 511--11 micro seconds = 1 degrees.
					--duty2		  : integer := 1500+200;--40 degrees
					--duty3		  : integer := 1500+300;--60 degrees
					--duty4		  : integer := 1500+400 --80 degrees
					);
					
			port(	clk_in	  :in std_logic;
					reset_n	  :in std_logic;
					sw0		  :in std_logic;
					position_x :in integer;
					position_y :in integer;
				 --  sw1		  :in std_logic;
				 --  sw2		  :in std_logic;
				 --  sw3		  :in std_logic;
					trigger		:in std_logic;
					ack_trigger		:out std_logic;
					pwm_out_y  :out std_logic;
					pwm_out_x	  :out std_logic
				 );
	end pwmg_servo;			 

architecture behav of pwmg_servo is	
 
	signal duty_0deg_x  : integer := 1250;
	signal duty_0deg_y  : integer := 1250;
	signal cnt   : integer := 0;
	signal duty_x: integer := duty_0deg_x -1 ;
	signal duty_y: integer := duty_0deg_y -1 ;
	signal new_cmd: integer := 1; 
	signal trigger_r: std_logic;
	
	
begin

	trigger_r <= trigger;
process(clk_in, reset_n, trigger)
	begin
	
		if (reset_n = '1') then
			cnt <= 0;
		elsif (rising_edge(clk_in)) then	
			if ( cnt = clk_divide - 1) then
				cnt <= 0;
			else
				cnt <= cnt + 1;
			end if;

			duty_x <= duty_0deg_x - position_x * 13 / 10;
			duty_y <= duty_0deg_y - position_y * 13 / 10;
		
			if trigger_r ='1' then		
				duty_0deg_x <= duty_x;
				duty_0deg_y <= duty_y;
				ack_trigger <= '1';
			else
				ack_trigger <= '0';
			end if;
		end if;
		
			
--		if is_first = '1' then
--			duty_x <= duty_0deg - position_x * 13 / 10;
--			duty_y <= duty_0deg - position_y * 13 / 10;
--			is_first <= '0';
			

--		else
--			duty_0deg_x <= duty_x;
--			duty_0deg_y <= duty_y;
			--new_cmd <= new_cmd + 1;
				
--				if ( (sw0 = '1') ) then
--					duty <= duty1 - 1;
--				elsif (sw0 = '0') then
--					duty <= duty_0deg - 1;
--				end if;
		
end process;

process(clk_in, reset_n)
	begin
	
		if ( cnt < duty_x ) then
			pwm_out_x <= '1';
		else
			pwm_out_x <= '0';
		end if;
		
		if ( cnt < duty_y ) then
			pwm_out_y <= '1';
		else
			pwm_out_y <= '0';
		end if;
		
end process;

end behav;


--the code that connects between the two programs

library ieee;
use ieee.std_logic_1164.all; 
use ieee.std_logic_unsigned.all;

entity pwmg_servo_full is 
	port( clk		  :in std_logic;
			clk_out    :buffer std_logic;
			reset		  :in std_logic;
			sw0		  :in std_logic;
			position_x :in integer;
			position_y :in integer;
			--sw1		  :in std_logic;
		  -- sw2		  :in std_logic;
			--sw3		  :in std_logic;
			trigger	  :in std_logic;
			ack_trigger:out std_logic;
			pwm_out_y  :out std_logic;
			pwm_out_x  :out std_logic 
		  );
end pwmg_servo_full;

architecture behav of pwmg_servo_full is

	component pwmg is
		port(	reset_n : in std_logic;
				clk_in  : in std_logic;	
				clk_out : out std_logic
			 );
	end component;
	
	component pwmg_servo is
		port(	clk_in	  :in std_logic;
				reset_n	  :in std_logic;
				sw0		  :in std_logic;
				position_x :in integer;
				position_y :in integer;
				trigger	  :in std_logic;
				ack_trigger:out std_logic;
			   pwm_out_y  :out std_logic;	
				pwm_out_x  :out std_logic
			 );
	end component;		 
	
	
	begin 
	
	inst_pwmg: pwmg 
		port map ( reset_n => reset,
					  clk_in => clk,
					  clk_out => clk_out
					);
	inst_pwmg_servo: pwmg_servo
		port map ( clk_in => clk_out,
					  reset_n => reset,
					  sw0 => sw0,
					  position_x => position_x,
					  position_y => position_y,
					  trigger => trigger,
					  ack_trigger => ack_trigger,
					  pwm_out_y => pwm_out_y,
					  pwm_out_x => pwm_out_x
					);
end behav;	



-- this design basically connects a CMOS camera (OV7670 module) to
-- DE2-115 board; video frames are picked up from camera, buffered
-- on the FPGA (using embedded RAM), and displayed on the VGA monitor,
-- which is also connected to the board; clock signals generated
-- inside FPGA using ALTPLL's that take as input the board's 50MHz signal
-- from on-board oscillator;
 
-- this whole project is an adaptation of Mike Field's original implementation 
-- that can be found here:
-- http://hamsterworks.co.nz/mediawiki/index.php/OV7670_camera
-- also, here in impl #2, we integrate an sdram controller to write and read
-- from sdram chip a whole frame; 
 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_unsigned.all;

entity digital_cam_impl2 is
  Port (
    slide_sw_RESET : in  STD_LOGIC; -- reset the whole animal;
    slide_sw_resend_reg_values : in  STD_LOGIC; -- rewrite all OV7670's registers;
    LED_config_finished : out STD_LOGIC; -- lets us know camera registers are now written;
    LED_dll_locked : out STD_LOGIC; -- PLL is locked now;
    btn_take_snapshot : in  STD_LOGIC; -- KEY0
    btn_display_snapshot1 : in  STD_LOGIC; -- KEY1
	 btn_rst_snapshot : in  STD_LOGIC; 
	 ledRed2 : out std_logic; -- received first frame
	 ledRed3 : out std_logic; -- received second frame
	 ledRed4 : out std_logic;
	 ledRed5 : out std_logic;
	 ledRed6 : out std_logic;
	 ledRed7 : out std_logic;
	 ledRed8 : buffer std_logic;
	 
    vga_hsync : out  STD_LOGIC;
    vga_vsync : out  STD_LOGIC;
    vga_r     : out  STD_LOGIC_vector(7 downto 0);
    vga_g     : out  STD_LOGIC_vector(7 downto 0);
    vga_b     : out  STD_LOGIC_vector(7 downto 0);
    vga_blank_N : out  STD_LOGIC;
    vga_sync_N  : out  STD_LOGIC;
    vga_CLK     : out  STD_LOGIC;

    ov7670_pclk  : in  STD_LOGIC;
    ov7670_xclk  : out STD_LOGIC;
    ov7670_vsync : in  STD_LOGIC;
    ov7670_href  : in  STD_LOGIC;
    ov7670_data  : in  STD_LOGIC_vector(7 downto 0);
    ov7670_sioc  : out STD_LOGIC;
    ov7670_siod  : inout STD_LOGIC;
    ov7670_pwdn  : out STD_LOGIC;
    ov7670_reset : out STD_LOGIC;
	 
	 sw2_take_snapshot1 		: in std_logic;
	 sw3_take_snapshot2		: in std_logic;
	 sw4_display_snapshot1	: in std_logic;
	 sw5_display_snapshot2	: in std_logic;
	 sw6_auto_enable			: in std_logic;
	 sw7_sub_display			: in std_logic;
	 sw8_sub_activate			: in std_logic;
	 sw9_thresh_activate		: in std_logic;
	 sw10_thresh_display		: in std_logic;
	 sw11_median_activate	: in std_logic;
	 sw12_median_display		: in std_logic;
	 sw13_all_activate		: in std_logic;
	 sw14_all_disp 			: in std_logic;
	 sw15_location_disp 		: in std_logic;
	 
	--clk 1mhz for servo
	 clk_out   :buffer std_logic;
	 pwm_out_y :out std_logic;
	 pwm_out_x :out std_logic;
    
    -- SDRAM related signals
    LED_snapshot_retrieved : out STD_LOGIC; -- snapshot just brought from SDRAM;
    DRAM_ADDR : out  STD_LOGIC_vector(12 downto 0); 
    DRAM_BA_0 : out  STD_LOGIC;
    DRAM_BA_1 : out  STD_LOGIC;
    DRAM_CAS_N : out  STD_LOGIC;
    DRAM_CKE : out  STD_LOGIC;
    DRAM_CLK : out  STD_LOGIC;
    DRAM_CS_N : out  STD_LOGIC;
    DRAM_DQ : inout  STD_LOGIC_vector(15 downto 0);
    DRAM_LDQM : out  STD_LOGIC; 
    DRAM_UDQM : out  STD_LOGIC;
    DRAM_RAS_N : out  STD_LOGIC;
    DRAM_WE_N : out  STD_LOGIC;
    
    finishLedOut,finishLedOut2,finishLedOut3,finishLedOut4,finishLedOut5,finishLedOut6,finishLedOut7 : out std_logic;    
	 clkIn :STD_LOGIC;
	 addressBufferOut:out std_logic_vector(17 downto 0);
	 enableBufferOut:out std_logic;
	 
	 lcdResetIn             : IN     std_logic;  -- Map this Port to a Switch within your [Port Declarations / Pin Planner]  
	 clock_50In           : IN     std_logic;  -- The DE2 50Mhz Clk and the "clk_count_400hz" counter variable are used to Genreate a 400Hz clock pulse                                               -- to drive the LCD CORE state machine.
	 lcd_rsOut             : OUT    std_logic;
	 lcd_eOut              : OUT    std_logic;
	 lcd_rwOut             : OUT    std_logic;
	 lcd_onOut             : OUT    std_logic;
	 lcd_blonOut           : OUT    std_logic;
	 data_bus_0Inout         : INOUT  STD_LOGIC;
	 data_bus_1Inout          : INOUT  STD_LOGIC;
	 data_bus_2Inout          : INOUT  STD_LOGIC;
	 data_bus_3Inout          : INOUT  STD_LOGIC;
	 data_bus_4Inout          : INOUT  STD_LOGIC;
	 data_bus_5Inout          : INOUT  STD_LOGIC;
	 data_bus_6Inout          : INOUT  STD_LOGIC;
	 data_bus_7Inout          : INOUT  STD_LOGIC;  

	 LCD_CHAR_ARRAY_3In    : IN    STD_LOGIC 
  );
end digital_cam_impl2;


architecture my_structural of digital_cam_impl2 is


  component pwmg_servo_full 
  	port( clk		  :in std_logic;
			clk_out    :buffer std_logic;
			reset		  :in std_logic;
			sw0		  :in std_logic;
			position_x :in integer;
			position_y :in integer;
			trigger		:in std_logic;
			ack_trigger :out std_logic;
			pwm_out_y  :out std_logic;
			pwm_out_x  :out std_logic 
		  );
	end component;


  COMPONENT gray_scale
  Port ( 
		clkIn,clk_i :STD_LOGIC;
		dataBufferIn:std_logic_vector(11 downto 0);
		dataBufferout:out std_logic_vector(11 downto 0)
  );
  end COMPONENT;

  COMPONENT sdram_rw_and_image_processing 
    Port ( 
      -- connections to sdram controller;
      clk_i : in  STD_LOGIC;
      rst_i : in  STD_LOGIC;
      addr_i : out  STD_LOGIC_vector(24 downto 0); 
      dat_i : out  STD_LOGIC_vector(31 downto 0);
      dat_o : in  STD_LOGIC_vector(31 downto 0);
      we_i : out  STD_LOGIC;
      ack_o : in  STD_LOGIC;
      stb_i : out  STD_LOGIC;
      cyc_i : out  STD_LOGIC;
      -- connections to frame buffer 2 for which we need to
      -- generate addresses and pass image data from SDRAM;
      addr_buf2 : OUT STD_LOGIC_VECTOR (17 downto 0);
      dout_buf2 : buffer std_logic_vector(11 downto 0);
      we_buf2 : OUT std_logic;
      -- connections from frame buffer 1 from where we 
      -- take snapshot; we of buf 1 is controlled by ov7670_capture
      -- here we only read from buffer 1 but at 50 MHz clock;
      addr_buf1 : OUT STD_LOGIC_VECTOR (17 downto 0);
      din_buf1 : IN std_logic_vector(11 downto 0);
		--greyscale
		din_imagepro: in std_logic_vector(11 downto 0);
      -- rw controls
      take_snapshot1 : in  STD_LOGIC;
	   take_snapshot2 : in  STD_LOGIC;	-- store to SDRAM;
		take_snapshot1_auto : buffer STD_LOGIC;
	   take_snapshot2_auto : buffer STD_LOGIC;
		btn_take_snapshot :in std_logic;
      display_snapshot1 : in  STD_LOGIC;
		display_snapshot2 : in  STD_LOGIC;	-- read/fetch from SDRAM;
		display_sub : in  STD_LOGIC;
		display_threshold : in  STD_LOGIC;
		display_median : in  STD_LOGIC;
		display_all : in  STD_LOGIC;
		display_location : in std_logic;
      led_done : out  STD_LOGIC;
		led_sub_done: out  STD_LOGIC;
	   numberToDisplay1:out std_logic_vector(17 downto 0);
	   numberToDisplay2:out std_logic_vector(17 downto 0);
		numberToDisplay3:out std_logic_vector(17 downto 0);
		numberToDisplay4:out std_logic_vector(17 downto 0);
		
	   sw2_take_snapshot1 		: in std_logic;
		sw3_take_snapshot2		: in std_logic;
		sw4_display_snapshot1	: in std_logic;
	   sw5_display_snapshot2	: in std_logic;
	   sw6_auto_enable			: in std_logic;
	   sw7_sub_display			: in std_logic;
		sw8_sub_activate			: in std_logic;
		sw9_thresh_activate		: in std_logic;
		sw10_thresh_display		: in std_logic;
		sw11_median_activate	: in std_logic;
		sw12_median_display		: in std_logic;
		sw13_all_activate		: in std_logic;
		sw14_all_disp : in std_logic;
		sw15_location_disp : in std_logic;
		
		frameOne :buffer std_logic;
		frameTwo : buffer std_logic;
		cmd_engine_x: buffer integer;
		cmd_engine_y: buffer integer;
		trigger: buffer std_logic;
		ack_trigger: in std_logic
    );
  end COMPONENT;

  COMPONENT sdram_controller 
    Port (
      clk_i: in  STD_LOGIC;
      dram_clk_i: in  STD_LOGIC;
      rst_i: in  STD_LOGIC;
      dll_locked: in  STD_LOGIC;
      -- all ddr signals
      dram_addr: out  STD_LOGIC_vector(12 downto 0); 
      dram_bank: out  STD_LOGIC_vector(1 downto 0);
      dram_cas_n: out  STD_LOGIC;
      dram_cke: out  STD_LOGIC;
      dram_clk: out  STD_LOGIC;
      dram_cs_n: out  STD_LOGIC;
      dram_dq: inout  STD_LOGIC_vector(15 downto 0);
      dram_ldqm: out  STD_LOGIC;
      dram_udqm: out  STD_LOGIC;
      dram_ras_n: out  STD_LOGIC;
      dram_we_n: out  STD_LOGIC;
		oe_r_r: buffer std_logic;
      -- wishbone bus
      addr_i: in  STD_LOGIC_vector(24 downto 0); 
      dat_i: in  STD_LOGIC_vector(31 downto 0);
      dat_o: out  STD_LOGIC_vector(31 downto 0);
      we_i: in  STD_LOGIC;
      ack_o: out  STD_LOGIC;
      stb_i: in  STD_LOGIC;
      cyc_i: in  STD_LOGIC
    );
  end COMPONENT;


  COMPONENT VGA
  PORT(
    CLK25 : IN std_logic;    
    Hsync : OUT std_logic;
    Vsync : OUT std_logic;
    Nblank : OUT std_logic;      
    clkout : OUT std_logic;
    activeArea : OUT std_logic;
    Nsync : OUT std_logic
    );
  END COMPONENT;

  COMPONENT ov7670_controller
  PORT(
    clk : IN std_logic;
    resend : IN std_logic;    
    siod : INOUT std_logic;      
    config_finished : OUT std_logic;
    sioc : OUT std_logic;
    reset : OUT std_logic;
    pwdn : OUT std_logic;
    xclk : OUT std_logic
    );
  END COMPONENT;

  COMPONENT frame_buffer
  PORT(
    data : IN std_logic_vector(11 downto 0);
    rdaddress : IN std_logic_vector(17 downto 0);
    rdclock : IN std_logic;
    wraddress : IN std_logic_vector(17 downto 0);
    wrclock : IN std_logic;
    wren : IN std_logic;          
    q : OUT std_logic_vector(11 downto 0)
    );
  END COMPONENT;

  COMPONENT ov7670_capture
  PORT(
    pclk : IN std_logic;
    vsync : IN std_logic;
    href : IN std_logic;
    d : IN std_logic_vector(7 downto 0);          
    addr : OUT std_logic_vector(17 downto 0);
    dout : OUT std_logic_vector(11 downto 0);
    we : OUT std_logic
    );
  END COMPONENT;

  COMPONENT RGB
  PORT(
    Din : IN std_logic_vector(11 downto 0);
    Nblank : IN std_logic;          
    R : OUT std_logic_vector(7 downto 0);
    G : OUT std_logic_vector(7 downto 0);
    B : OUT std_logic_vector(7 downto 0)
    );
  END COMPONENT;

  COMPONENT Address_Generator
  PORT(
    rst_i : in std_logic;
    CLK25       : IN  std_logic;
    enable      : IN  std_logic;       
    vsync       : in  STD_LOGIC;
    address     : OUT std_logic_vector(17 downto 0)
    );
  END COMPONENT;

  COMPONENT debounce
    Port ( 
      clk : in  STD_LOGIC;
      i : in  STD_LOGIC;
      o : out  STD_LOGIC);
  end COMPONENT;
  
  -- DE2-115 board has an Altera Cyclone V E, which has ALTPLLs;
  COMPONENT my_altpll
  PORT
  (
    areset    : IN STD_LOGIC := '0';
    inclk0    : IN STD_LOGIC := '0';
    c0    : OUT STD_LOGIC ;
    c1    : OUT STD_LOGIC ;
    c2    : OUT STD_LOGIC ;
    c3    : OUT STD_LOGIC ;
    locked    : OUT STD_LOGIC 
  );
  END COMPONENT;
 
 	component LCD_DISPLAY_nty port (
		reset                	: IN     std_logic;  -- Map this Port to a Switch within your [Port Declarations / Pin Planner]  
		clock_50         	   	: IN     std_logic;  -- The DE2 50Mhz Clk and the "clk_count_400hz"  variable are used to Genreate a 400Hz clock pulse                                               -- to drive the LCD CORE state machine.
		lcd_rs             		: OUT    std_logic;
		lcd_e                	: OUT    std_logic;
		lcd_rw             		: OUT    std_logic;
		lcd_on             		: OUT    std_logic;
		lcd_blon           		: OUT    std_logic;
		data_bus_0         		: INOUT  STD_LOGIC;
		data_bus_1         		: INOUT  STD_LOGIC;
		data_bus_2         		: INOUT  STD_LOGIC;
		data_bus_3         		: INOUT  STD_LOGIC;
		data_bus_4     	      : INOUT  STD_LOGIC;
		data_bus_5         		: INOUT  STD_LOGIC;
		data_bus_6         		: INOUT  STD_LOGIC;
		data_bus_7         		: INOUT  STD_LOGIC;  
		number_to_display1		: in integer;
		number_to_display2		: in integer;
		number_to_display3		: in integer;
		number_to_display4		: in integer;
		sw2_take_snapshot1 		: in std_logic;
		sw3_take_snapshot2		: in std_logic;
		sw4_display_snapshot1	: in std_logic;
		sw5_display_snapshot2	: in std_logic;
		sw6_auto_enable			: in std_logic;
		sw7_sub_display			: in std_logic;
		sw8_sub_activate			: in std_logic;
		sw9_thresh_activate		: in std_logic;
		sw10_thresh_display		: in std_logic;
		sw11_median_activate	: in std_logic;
		sw12_median_display		: in std_logic;
		sw13_all_activate		: in std_logic;
		sw14_all_disp 			: in std_logic;
		sw15_location_disp 		: in std_logic;
		LCD_CHAR_ARRAY_3    		: IN    STD_LOGIC      
		);
	end component;

  -- use the Altera MegaWizard to generate the ALTPLL module; generate 3 clocks, 
  -- clk0 @ 100 MHz
  -- clk1 @ 100 MHz with a phase adjustment of -3ns
  -- clk2 @ 50 MHz and 
  -- clk3 @ 25 MHz 

  signal clk_100 : std_logic;       -- clk0: 100 MHz
  signal clk_100_3ns : std_logic;   -- clk1: 100 MHz with phase adjustment of -3ns
  signal clk_50_camera : std_logic; -- clk2: 50 MHz
  signal clk_25_vga : std_logic;    -- clk3: 25 MHz
  signal dll_locked : std_logic;
  signal snapshot_done : std_logic;

  -- buffers 
  signal wren_buf1_from_ov7670_capture : std_logic;
  signal wren_buf_1 : std_logic;
  signal wren_buf1_from_imageProcessing : std_logic;
  signal wraddress_buf_1 : std_logic_vector(17 downto 0);
  signal wrdata_buf_1 : std_logic_vector(11 downto 0); 
  signal rddata_buf_1 : std_logic_vector(11 downto 0);
  signal rddata_buf_1_from_imageProcessing : std_logic_vector(11 downto 0);
  

  signal wren_buf_2 : std_logic;
  signal wraddress_buf_2 : std_logic_vector(17 downto 0);
  signal wrdata_buf_2 : std_logic_vector(11 downto 0); 
  signal rddata_buf_2 : std_logic_vector(11 downto 0);

  -- when in video mode, rd address comes from address_generator and rd clock is 25 MHz
  -- when take snapshot mode, rd address comes from  and rd clock is 25 MHz;
  signal rdaddress_buf_1 : std_logic_vector(17 downto 0);
  signal rdaddress_buf_2 : std_logic_vector(17 downto 0);
  signal rdaddress_from_sdram_rw_and_image_processing : std_logic_vector(17 downto 0);
  signal rdaddress_from_addr_gen : std_logic_vector(17 downto 0);
  signal rdaddress_buf_1_from_imageProcessing : std_logic_vector(17 downto 0); 
  signal frameOne : std_logic;
  signal frameTwo : std_logic;
  signal ready: std_logic;
  -- user controls;
  signal resend_reg_values : std_logic;
  signal take_snapshot1 : std_logic;
  signal take_snapshot2 : std_logic;
  signal take_snapshot1_auto : std_logic;
  signal take_snapshot2_auto : std_logic;
  signal take_snapshot_synchronized1 : std_logic := '0';
  signal take_snapshot_synchronized2 : std_logic := '0';
  signal display_snapshot1 : std_logic;
  signal display_snapshot2 : std_logic;

  signal display_sub : std_logic;
  signal display_threshold : std_logic;
  signal display_median : std_logic;
  signal display_all : std_logic;
  signal display_location : std_logic;
  signal display_snapshot_synchronized1 : std_logic := '0';
  signal display_snapshot_synchronized2 : std_logic := '0';
  signal display_sub_synchronized : std_logic := '0';
  signal display_threshold_synchronized : std_logic := '0';
  signal display_median_synchronized : std_logic := '0';
  signal display_all_synchronized : std_logic := '0';
  signal display_location_synchronized : std_logic := '0';

  signal reset_global : std_logic;
  signal reset_manual : std_logic; -- by the user;
  signal reset_automatic : std_logic;
  signal reset_sdram_interface : std_logic;

  -- RGB related;
  signal red,green,blue : std_logic_vector(7 downto 0);
  signal activeArea : std_logic;
  signal nBlank     : std_logic;
  signal vSync      : std_logic;
  -- data_to_rgb should the multiplexing of rddata_buf_1 (when displaying
  -- video directly) or rddata_buf_2 (when displaying the contents of buffer 2
  -- which has snapshot taken earlier and saved in SDRAM or some "processed"
  -- version of that snapshot);
  signal data_to_rgb : std_logic_vector(11 downto 0);

  --- SDRAM relted;
  signal dram_bank: std_logic_vector(1 downto 0);
  signal addr_i: std_logic_vector(24 downto 0);
  signal dat_i: std_logic_vector(31 downto 0);
  signal dat_o: std_logic_vector(31 downto 0);
  signal we_i: std_logic;
  signal ack_o: std_logic;
  signal stb_i: std_logic;
  signal cyc_i: std_logic;
  signal numberToDisplay1,numberToDisplay2,numberToDisplay3,numberToDisplay4: std_logic_vector(17 downto 0);
  signal rstIn: std_logic; 
  
  --imageprocessing to sdam related
  signal dataBufferout_r: std_logic_vector(11 downto 0);
  signal oe_r: std_logic;
  signal cmd_engine_x : integer;
  signal cmd_engine_y : integer;
  signal trigger: std_logic;
  signal ack_trigger: std_logic;

  	--LCD
	signal     LCDReset              : std_logic;  -- Map this Port to a Switch within your [Port Declarations / Pin Planner]  
	signal      clock_50           :std_logic;  -- The DE2 50Mhz Clk and the "clk_count_400hz" counter variable are used to Genreate a 400Hz clock pulse                                               -- to drive the LCD CORE state machine.
	signal      lcd_rs             : std_logic;
	signal      lcd_e              : std_logic;
	signal      lcd_rw             : std_logic;
	signal      lcd_on             : std_logic;
	signal      lcd_blon           : std_logic;
	signal      data_bus_0         : STD_LOGIC;
	signal      data_bus_1         : STD_LOGIC;
	signal     data_bus_2         : STD_LOGIC;
	signal     data_bus_3         : STD_LOGIC;
	signal      data_bus_4         : STD_LOGIC;
	signal      data_bus_5         : STD_LOGIC;
	signal      data_bus_6         : STD_LOGIC;
	signal      data_bus_7         : STD_LOGIC;  
	signal      LCD_CHAR_ARRAY_0    : STD_LOGIC;
	signal      LCD_CHAR_ARRAY_1    : STD_LOGIC;
	signal      LCD_CHAR_ARRAY_2    : STD_LOGIC;
	signal      LCD_CHAR_ARRAY_3    : STD_LOGIC;

  
begin

  -- take the inverted push buttons because KEY# on DE2-115 board generates
  -- a signal 111000111; with 1 with not pressed and 0 when pressed/pushed;

  take_snapshot1 <= take_snapshot1_auto when (sw6_auto_enable = '1' and sw13_all_activate ='1') else sw2_take_snapshot1;
  take_snapshot2 <= take_snapshot2_auto when (sw6_auto_enable = '1' and sw13_all_activate ='1') else sw3_take_snapshot2;
  display_snapshot1 <= sw4_display_snapshot1;
  display_snapshot2 <= sw5_display_snapshot2;
  display_sub <= sw7_sub_display;
  display_threshold <= sw10_thresh_display;
  display_median <= sw12_median_display;
  display_all <= sw14_all_disp;
  display_location <= sw15_location_disp;
  vga_r <= red(7 downto 0);
  vga_g <= green(7 downto 0);
  vga_b <= blue(7 downto 0);
  vga_vsync <= vsync;
  vga_blank_N <= nBlank;
  ledRed2 <= '1' when (frameOne='1' or frameTwo='1') else '0';
  ledRed3 <= '1' when (frameOne='1' and frameTwo='1') and (ledRed8='0') else '0';
  ledRed4 <= '1' when (frameOne='1' and frameTwo='1') else '0';
  ledRed5 <= '1' when frameOne='1' else '0';
  ledRed6 <= '1' when frameTwo='1' else '0';
  ready <= '1' when (frameOne='1' and frameTwo='1') else '0';
  LED_dll_locked <= '1'; -- dll_locked; -- LEDRed[o] notifies user;
  LED_snapshot_retrieved <= snapshot_done;

	lcdDisplay: LCD_DISPLAY_nty port map( 
	reset=>lcdResetIn ,  -- Map this Port to a Switch within your [Port Declarations / Pin Planner]  
	clock_50=>clock_50In ,  -- The DE2 50Mhz Clk and the "clk_count_400hz" counter variable are used to Genreate a 400Hz clock pulse                                               -- to drive the LCD CORE state machine.
	lcd_rs=>lcd_rsOut ,
	lcd_e=>lcd_eOut ,
	lcd_rw=>lcd_rwOut ,
	lcd_on=>lcd_onOut ,
	lcd_blon=>lcd_blonOut ,
	data_bus_0=>data_bus_0Inout ,
	data_bus_1=>data_bus_1Inout ,
	data_bus_2=>data_bus_2Inout ,
	data_bus_3=>data_bus_3Inout ,
	data_bus_4=>data_bus_4Inout ,
	data_bus_5=>data_bus_5Inout ,
	data_bus_6=>data_bus_6Inout ,
	data_bus_7=>data_bus_7Inout , 
	number_to_display1=>to_integer(unsigned(numberToDisplay1)) ,
	number_to_display2=>to_integer(unsigned(numberToDisplay2)) ,
	number_to_display3=>to_integer(unsigned(numberToDisplay3)) ,
	number_to_display4=>to_integer(unsigned(numberToDisplay4)) ,
	sw2_take_snapshot1=>sw2_take_snapshot1,
	sw3_take_snapshot2=>sw3_take_snapshot2,
	sw4_display_snapshot1=>sw4_display_snapshot1,
	sw5_display_snapshot2=>sw5_display_snapshot2,
	sw6_auto_enable=>sw6_auto_enable,
	sw7_sub_display=>sw7_sub_display,
	sw8_sub_activate=>sw8_sub_activate,
	sw9_thresh_activate=>sw9_thresh_activate,
	sw10_thresh_display=>sw10_thresh_display,
	sw11_median_activate=>sw11_median_activate,
	sw12_median_displaY=>sw12_median_display,
	sw13_all_activate=>sw13_all_activate,
	sw14_all_disp =>sw14_all_disp,
	sw15_location_disp=>sw15_location_disp,
	LCD_CHAR_ARRAY_3=>LCD_CHAR_ARRAY_3In 
	);  
  
inst_pwmg_servo_full: pwmg_servo_full port map(
	clk => clock_50In,
	clk_out => clk_out,
	reset => slide_sw_RESET,
	sw0 => LCD_CHAR_ARRAY_3In,
	position_x => cmd_engine_x,
	position_y => cmd_engine_y,
	trigger => trigger,
	ack_trigger => ack_trigger,
	pwm_out_y => pwm_out_y,
	pwm_out_x => pwm_out_x
	);
	
  
 
inst_gray_scale: gray_scale port map(
   clkIn => clk_25_vga,
	clk_i => clk_100,
   dataBufferIn => rddata_buf_1,
	dataBufferout => dataBufferout_r
); 	
  
  -- clocks generation;
  Inst_four_clocks_pll: my_altpll PORT MAP(
    areset => '0', -- reset_general?
    inclk0 => clock_50In,
    c0 => clk_100,
    c1 => clk_100_3ns,
    c2 => clk_50_camera,
    c3 => clk_25_vga,
    locked => dll_locked -- drives an LED and SDRAM controller;
  );
  
  -- debouncing slide switches;
  -- take entity input slide_sw_resend_reg_values and debounce it to
  -- get clean resend_reg_values signal;
  -- same for debouncing of slide_sw_RESET to get clean reset_general;
  Inst_debounce_resend: debounce PORT MAP(
    clk => clk_25_vga,
    i   => slide_sw_resend_reg_values,
    o   => resend_reg_values
  );  
  Inst_debounce_reset: debounce PORT MAP(
    clk => clk_25_vga,
    i   => slide_sw_RESET,
    o   => reset_manual
  );  
  -- first thing when the system is powered on, I should automatically
  -- reset everything for a few clock cycles;
  reset_automatic <= '0'; -- for the time being;
  reset_global <= (reset_manual or reset_automatic);
  
  -- generate pulse signals only when vsync is '0' to take frame
  -- synchronized with the beginning of it; otherwise, pixels may be picked-up 
  -- from different frames;
  take_snapshot_synchronized1 <= take_snapshot1 and (not vsync);
  take_snapshot_synchronized2 <= take_snapshot2 and (not vsync);
  display_snapshot_synchronized1 <= display_snapshot1 and (not vsync);
  display_snapshot_synchronized2 <= display_snapshot2 and (not vsync);
  display_sub_synchronized <= display_sub and (not vsync);
  display_threshold_synchronized <= display_threshold and (not vsync);
  display_median_synchronized <= display_median and (not vsync);
  display_all_synchronized <= display_all and (not vsync);
  display_location_synchronized <= display_location and (not vsync);

  -- implementing muxes for rdaddress inputs of the frame buffers;
  process (clk_100)
  begin
    if rising_edge (clk_100) then
      if (take_snapshot1 = '1') then
        wren_buf_1 <= '0'; -- disable writing into buffer 1 while taking a snapshot;
        rdaddress_buf_1 <= rdaddress_from_sdram_rw_and_image_processing; -- comes from sdram_rw_and_image_processing entity;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2; -- simple multiplexer to pick-up out of buffer 1 or out of buffer 2;
      elsif (display_snapshot1 = '1') then
        wren_buf_1 <= '0';
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2; 
      elsif (take_snapshot2 = '1') then
        wren_buf_1 <= '0'; -- disable writing into buffer 1 while taking a snapshot;
        rdaddress_buf_1 <= rdaddress_from_sdram_rw_and_image_processing; -- comes from sdram_rw_and_image_processing entity;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2; -- simple multiplexer to pick-up out of buffer 1 or out of buffer 2;
      elsif (display_snapshot2 = '1') then
        wren_buf_1 <= '0';
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2; 
      elsif (display_sub = '1') then
        wren_buf_1 <= '0';
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2; 
		elsif (display_threshold = '1') then
        wren_buf_1 <= '0';
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2;
		elsif (display_median = '1') then
        wren_buf_1 <= '0';
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2;
		elsif (display_all = '1') then
        wren_buf_1 <= '0';
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2;
		elsif (display_location = '1') then
        wren_buf_1 <= '0';
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen;
        data_to_rgb <= rddata_buf_2;
		else
        wren_buf_1 <= wren_buf1_from_ov7670_capture;
        rdaddress_buf_1 <= rdaddress_from_addr_gen;
        rdaddress_buf_2 <= rdaddress_from_addr_gen; 
        data_to_rgb <= rddata_buf_1; 
      end if;
    end if;
  end process;
  -- process resposible with generating reset pulse on falling edge of take_snapshot
  -- or display_snapshot1; 
  process (clk_100)
  begin
    if rising_edge (clk_100) then
      if (reset_global = '1') then
        reset_sdram_interface <= '1';
       elsif (((take_snapshot1 = '0' and display_snapshot1 = '0') and (take_snapshot2 = '0' and display_snapshot2 = '0')) and display_sub = '0' and display_threshold = '0' and display_median ='0' and display_all = '0' and display_location = '0' and (snapshot_done = '1')) then
        reset_sdram_interface <= '1';
      else
        reset_sdram_interface <= '0';
      end if;
    end if;
  end process;
  
  
  -- video frames are buffered into buffer_1; from here frames are "pipelined"
  -- into the VGA thing; also from buffer_1 we take one frame and
  -- save into SDRAM when push button KEY0 is pressed - that means "take snapshot!";
  Inst_frame_buf_1: frame_buffer PORT MAP(
    rdaddress => rdaddress_buf_1,
    rdclock   => clk_25_vga, 
    q         => rddata_buf_1, -- goes to data_to_rgb thru mux;    
    wrclock   => ov7670_pclk, -- clock from camera module;
    wraddress => wraddress_buf_1,
    data      => wrdata_buf_1,
    wren      => wren_buf_1
  );
  -- buffer 2 is used to bring frame from sdram; and displayed then on VGA thing;
  Inst_frame_buf_2: frame_buffer PORT MAP(
    rdaddress => rdaddress_buf_2,
    rdclock   => clk_25_vga,
    q         => rddata_buf_2, -- goes to data_to_rgb thru mux;    
    wrclock   => clk_25_vga, 
    wraddress => wraddress_buf_2,
    data      => wrdata_buf_2,
    wren      => wren_buf_2
  );  
  

  -- camera module related blocks;
  Inst_ov7670_controller: ov7670_controller PORT MAP(
    clk             => clk_50_camera,
    resend          => resend_reg_values, -- debounced;
    config_finished => LED_config_finished, -- LEDRed[1] notifies user;
    sioc            => ov7670_sioc,
    siod            => ov7670_siod,
    reset           => ov7670_reset,
    pwdn            => ov7670_pwdn,
    xclk            => ov7670_xclk
  );
   
  Inst_ov7670_capture: ov7670_capture PORT MAP(
    pclk  => ov7670_pclk,
    vsync => ov7670_vsync,
    href  => ov7670_href,
    d     => ov7670_data,
    addr  => wraddress_buf_1,
    dout  => wrdata_buf_1,
    we    => wren_buf1_from_ov7670_capture -- goes to wren_buf_1;
  );
  
  
  -- VGA related stuff;
  Inst_VGA: VGA PORT MAP(
    CLK25      => clk_25_vga,
    clkout     => vga_CLK,
    Hsync      => vga_hsync,
    Vsync      => vsync,
    Nblank     => nBlank,
    Nsync      => vga_sync_N,
    activeArea => activeArea
  );  
  
  Inst_RGB: RGB PORT MAP(
    Din => data_to_rgb, -- comes from rddata_buf_1 or rddata_buf_2;
    Nblank => activeArea,
    R => red,
    G => green,
    B => blue
  );

  Inst_Address_Generator: Address_Generator PORT MAP(
    rst_i => '0',
    CLK25 => clk_25_vga,
    enable => activeArea,
    vsync => vsync,
    address => rdaddress_from_addr_gen
  );
  
  -- SDRAM related;
  DRAM_BA_1 <= dram_bank(1);
  DRAM_BA_0 <= dram_bank(0);
  
  Inst_sdram_controller: sdram_controller PORT MAP (
    clk_i => clk_100,
    dram_clk_i => clk_100_3ns,
    rst_i => reset_sdram_interface,
    dll_locked => dll_locked,
    -- all sdram signals
    dram_addr => DRAM_ADDR,
    dram_bank => dram_bank,
    dram_cas_n => DRAM_CAS_N,
    dram_cke => DRAM_CKE,
    dram_clk => DRAM_CLK,
    dram_cs_n => DRAM_CS_N,
    dram_dq => DRAM_DQ,
    dram_ldqm => DRAM_LDQM,
    dram_udqm => DRAM_UDQM,
    dram_ras_n => DRAM_RAS_N,
    dram_we_n => DRAM_WE_N,
	 oe_r_r => oe_r,
    -- wishbone bus;
    addr_i => addr_i,
    dat_i => dat_i,
    dat_o => dat_o,
    we_i => we_i,
    ack_o => ack_o,
    stb_i => stb_i,
    cyc_i => cyc_i
  );

  -- Note: here I use take_snapshot_synchronized and display_snapshot_synchronized1
  -- so that taking or displaying is done once only for a press of the push buttons,
  -- no matter how long by the user;
  -- however, once the user released the pushbutton, the system should be ready to take 
  -- another shot again; to achieve that, I apply an automatic reset pulse,
  -- once take_snapshot and display_snapshot1 are released, i.e., they go back to '0';
  Inst_sdram_rw_and_image_processing: sdram_rw_and_image_processing PORT MAP (
    -- connections to sdram controller;
    clk_i => clk_25_vga,
    rst_i => reset_sdram_interface,
    addr_i => addr_i,
    dat_i => dat_i,
    dat_o => dat_o,
    we_i => we_i,
    ack_o => ack_o,
    stb_i => stb_i,
    cyc_i => cyc_i,
    -- connections to frame buffer 2 
    addr_buf2 => wraddress_buf_2,
    dout_buf2 => wrdata_buf_2,
    we_buf2 => wren_buf_2,
    -- connections from frame buffer 1
    addr_buf1 => rdaddress_from_sdram_rw_and_image_processing,
    din_buf1 => rddata_buf_1,
	 --grשyscale
	 din_imagepro => dataBufferout_r,
    -- rw controls
    take_snapshot1 => take_snapshot_synchronized1,
	 take_snapshot2 => take_snapshot_synchronized2,
	 take_snapshot1_auto => take_snapshot1_auto,
	 take_snapshot2_auto => take_snapshot2_auto,
	 btn_take_snapshot => btn_take_snapshot,
    display_snapshot1 => display_snapshot_synchronized1,
	 display_snapshot2 => display_snapshot_synchronized2,
	 display_sub => display_sub_synchronized,
	 display_threshold => display_threshold_synchronized,
	 display_median => display_median_synchronized,
	 display_all => display_all_synchronized,
	 display_location =>display_location_synchronized,
	 led_done => snapshot_done, -- notify user on LEDGreen[1] that image from/to SDRAM is finished;
	 led_sub_done=> ledRed8,
	 numberToDisplay1=>numberToDisplay1,
	 numberToDisplay2=>numberToDisplay2,
	 numberToDisplay3=>numberToDisplay3,
	 numberToDisplay4=>numberToDisplay4,
	 sw2_take_snapshot1=>sw2_take_snapshot1,
	 sw3_take_snapshot2=>sw3_take_snapshot2,
	 sw4_display_snapshot1=>sw4_display_snapshot1,
	 sw5_display_snapshot2=>sw5_display_snapshot2,
	 sw6_auto_enable=>sw6_auto_enable,
	 sw7_sub_display=>sw7_sub_display,
	 sw8_sub_activate => sw8_sub_activate,
	 sw9_thresh_activate => sw9_thresh_activate,
	 sw10_thresh_display => sw10_thresh_display,
	 sw11_median_activate => sw11_median_activate,
	 sw12_median_display => sw12_median_display,
	 sw13_all_activate => sw13_all_activate,
	 sw14_all_disp => sw14_all_disp,
	 sw15_location_disp => sw15_location_disp,
	 frameOne=>frameOne,
	 frameTwo=>frameTwo,
	 cmd_engine_x => cmd_engine_x,
	 cmd_engine_y => cmd_engine_y,
	 trigger => trigger,
	 ack_trigger => ack_trigger
	 
  );  
  
end my_structural;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity Address_Generator is
  Port ( 
    rst_i : in std_logic;
    CLK25 : in STD_LOGIC;  -- horloge de 25 MHz et signal d'activation respectivement
    enable : in STD_LOGIC;
    vsync : in STD_LOGIC;
    address : out STD_LOGIC_VECTOR (17 downto 0) -- adresse genere
  );  
end Address_Generator;


architecture Behavioral of Address_Generator is

  signal val: STD_LOGIC_VECTOR(address'range) := (others => '0'); -- signal intermidiaire
  
begin

  address <= val; -- adresse genere

  process(CLK25)
  begin
    if rising_edge(CLK25) then
    
      if (rst_i = '1') then
        val <= (others => '0'); 
      else 
        if (enable='1') then      -- si enable = 0 on arrete la generation d'adresses
          if (val < 320*240) then -- si l'espace memoire est balay completement        
            val <= val + 1 ;
          end if;
        end if;
        if vsync = '0' then 
           val <= (others => '0');
        end if;        
      end if;
      
    end if;  
  end process;
    
end Behavioral;



-- simple debouncing technique; needs to be used for any slide switch
-- of the DE2-115;
-- TODO: should use the debouncing entity from Pong P. Chus' book; 
-- it's more elegant;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity debounce is
  Port ( 
    clk : in  STD_LOGIC;
    i : in  STD_LOGIC;
    o : out  STD_LOGIC
  );
end debounce;


architecture Behavioral of debounce is

  signal c : unsigned(23 downto 0);

begin

  process(clk)
  begin
    if rising_edge(clk) then
      if i = '1' then
        if c = x"FFFFFF" then
          o <= '1';
        else
          o <= '0';
        end if;
        c <= c + 1;
      else
        c <= (others => '0');
        o <= '0';
      end if;
    end if;
  end process;

end Behavioral;



-- create a buffer to store pixels data for a frame of 320x240 pixels;
-- data for each pixel is 12 bits;
-- that is 76800 pixels; hence, address is represented on 17 bits 
-- (2^17 = 131072 > 76800);
-- Notes: 
-- 1) If we wanted to work with 640x480 pixels, that would require
-- an amount of embedded RAM that is not available on the Cyclone IV E of DE2-115;
-- 2) We create the buffer with 76800 by stacking-up two blocks
-- of 2^16 = 65536 addresses; 

LIBRARY ieee;
USE ieee.std_logic_1164.all;


ENTITY frame_buffer IS
  PORT (
    data     : IN STD_LOGIC_VECTOR (11 DOWNTO 0);
    rdaddress : IN STD_LOGIC_VECTOR (17 downto 0);
    rdclock   : IN STD_LOGIC;
    wraddress : IN STD_LOGIC_VECTOR (17 downto 0);
    wrclock   : IN STD_LOGIC;
    wren     : IN STD_LOGIC;
    q        : OUT STD_LOGIC_VECTOR (11 DOWNTO 0)
  );
END frame_buffer;


ARCHITECTURE SYN OF frame_buffer IS

  
  COMPONENT my_frame_buffer_15to0 IS
  PORT
  (
    data    : IN STD_LOGIC_VECTOR (11 DOWNTO 0);
    rdaddress    : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    rdclock    : IN STD_LOGIC ;
    wraddress    : IN STD_LOGIC_VECTOR (15 DOWNTO 0);
    wrclock    : IN STD_LOGIC := '1';
    wren    : IN STD_LOGIC := '0';
    q    : OUT STD_LOGIC_VECTOR (11 DOWNTO 0)
  );
  END COMPONENT;

  
  -- read signals
  signal q_top : STD_LOGIC_VECTOR (11 DOWNTO 0);
  signal q_bottom : STD_LOGIC_VECTOR (11 DOWNTO 0);
  -- write signals
  signal wren_top : STD_LOGIC;
  signal wren_bottom : STD_LOGIC;
  
BEGIN

  Inst_buffer_top : my_frame_buffer_15to0
    PORT MAP (
      data => data(11 downto 0),
      rdaddress => rdaddress(15 downto 0),
      rdclock => rdclock,
      wraddress => wraddress(15 downto 0),
      wrclock => wrclock,
      wren => wren_top,
      q => q_top
    );
  Inst_buffer_bottom : my_frame_buffer_15to0
    PORT MAP (
      data => data(11 downto 0),
      rdaddress => rdaddress(15 downto 0),
      rdclock => rdclock,
      wraddress => wraddress(15 downto 0),
      wrclock => wrclock,
      wren => wren_bottom,
      q => q_bottom
    );  
    
  process (wraddress(16), wren)
  begin
      case wraddress(16) is 
        when '0' =>
          wren_top <= wren; wren_bottom <= '0';
        when '1' =>
          wren_top <= '0'; wren_bottom <= wren;  
        when others =>
          wren_top <= '0'; wren_bottom <= '0';
      end case;
  end process;
  
  process (rdaddress(16), q_top, q_bottom)
  begin
      case rdaddress(16) is 
        when '0' =>
          q <= q_top;
        when '1' =>
          q <= q_bottom;
        when others =>
          q <= "000000000000";
      end case;
  end process;
    
END SYN;



-- this is an entity used to sSend the commands to the OV7670 camera module
-- over an I2C-like interface

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity i2c_sender is
  Port ( clk   : in  STD_LOGIC;   
    siod  : inout  STD_LOGIC;
    sioc  : out  STD_LOGIC;
    taken : out  STD_LOGIC;
    send  : in  STD_LOGIC;
    id    : in  STD_LOGIC_VECTOR (7 downto 0);
    reg   : in  STD_LOGIC_VECTOR (7 downto 0);
    value : in  STD_LOGIC_VECTOR (7 downto 0));
end i2c_sender;


architecture Behavioral of i2c_sender is

  signal   divider  : unsigned (7 downto 0) := "00000001"; -- this value gives a 254 cycle pause before the initial frame is sent
  signal   busy_sr  : std_logic_vector(31 downto 0) := (others => '0');
  signal   data_sr  : std_logic_vector(31 downto 0) := (others => '1');
  
begin

  process(busy_sr, data_sr(31))
  begin
    if busy_sr(11 downto 10) = "10" or 
       busy_sr(20 downto 19) = "10" or 
       busy_sr(29 downto 28) = "10"  then
      siod <= 'Z';
    else
      siod <= data_sr(31);
    end if;
  end process;
  
  process(clk)
  begin
    if rising_edge(clk) then
      taken <= '0';
      if busy_sr(31) = '0' then
        SIOC <= '1';
        if send = '1' then
          if divider = "00000000" then
            data_sr <= "100" &   id & '0'  &   reg & '0' & value & '0' & "01";
            busy_sr <= "111" & "111111111" & "111111111" & "111111111" & "11";
            taken <= '1';
          else
            divider <= divider+1; -- this only happens on powerup
          end if;
        end if;
      else

        case busy_sr(32-1 downto 32-3) & busy_sr(2 downto 0) is
          when "111"&"111" => -- start seq #1
            case divider(7 downto 6) is
              when "00"   => SIOC <= '1';
              when "01"   => SIOC <= '1';
              when "10"   => SIOC <= '1';
              when others => SIOC <= '1';
            end case;
          when "111"&"110" => -- start seq #2
            case divider(7 downto 6) is
              when "00"   => SIOC <= '1';
              when "01"   => SIOC <= '1';
              when "10"   => SIOC <= '1';
              when others => SIOC <= '1';
            end case;
          when "111"&"100" => -- start seq #3
            case divider(7 downto 6) is
              when "00"   => SIOC <= '0';
              when "01"   => SIOC <= '0';
              when "10"   => SIOC <= '0';
              when others => SIOC <= '0';
            end case;
          when "110"&"000" => -- end seq #1
            case divider(7 downto 6) is
              when "00"   => SIOC <= '0';
              when "01"   => SIOC <= '1';
              when "10"   => SIOC <= '1';
              when others => SIOC <= '1';
            end case;
          when "100"&"000" => -- end seq #2
            case divider(7 downto 6) is
              when "00"   => SIOC <= '1';
              when "01"   => SIOC <= '1';
              when "10"   => SIOC <= '1';
              when others => SIOC <= '1';
            end case;
          when "000"&"000" => -- Idle
            case divider(7 downto 6) is
              when "00"   => SIOC <= '1';
              when "01"   => SIOC <= '1';
              when "10"   => SIOC <= '1';
              when others => SIOC <= '1';
            end case;
          when others      => 
            case divider(7 downto 6) is
              when "00"   => SIOC <= '0';
              when "01"   => SIOC <= '1';
              when "10"   => SIOC <= '1';
              when others => SIOC <= '0';
            end case;
        end case;   

        if divider = "11111111" then
          busy_sr <= busy_sr(32-2 downto 0) & '0';
          data_sr <= data_sr(32-2 downto 0) & '1';
          divider <= (others => '0');
        else
          divider <= divider+1;
        end if;
      end if;
    end if;
  end process;
end Behavioral;



-- megafunction wizard: %ALTPLL%
-- GENERATION: STANDARD
-- VERSION: WM1.0
-- MODULE: altpll 

-- ============================================================
-- File Name: my_altpll.vhd
-- Megafunction Name(s):
-- 			altpll
--
-- Simulation Library Files(s):
-- 			altera_mf
-- ============================================================
-- ************************************************************
-- THIS IS A WIZARD-GENERATED FILE. DO NOT EDIT THIS FILE!
--
-- 13.1.0 Build 162 10/23/2013 SJ Web Edition
-- ************************************************************


--Copyright (C) 1991-2013 Altera Corporation
--Your use of Altera Corporation's design tools, logic functions 
--and other software and tools, and its AMPP partner logic 
--functions, and any output files from any of the foregoing 
--(including device programming or simulation files), and any 
--associated documentation or information are expressly subject 
--to the terms and conditions of the Altera Program License 
--Subscription Agreement, Altera MegaCore Function License 
--Agreement, or other applicable license agreement, including, 
--without limitation, that your use is for the sole purpose of 
--programming logic devices manufactured by Altera and sold by 
--Altera or its authorized distributors.  Please refer to the 
--applicable agreement for further details.


LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY altera_mf;
USE altera_mf.all;

ENTITY my_altpll IS
	PORT
	(
		areset		: IN STD_LOGIC := '0';
		inclk0		: IN STD_LOGIC := '0';
		c0		: OUT STD_LOGIC ;
		c1		: OUT STD_LOGIC ;
		c2		: OUT STD_LOGIC ;
		c3		: OUT STD_LOGIC ;
		locked		: OUT STD_LOGIC 
	);
END my_altpll;


ARCHITECTURE SYN OF my_altpll IS

	SIGNAL sub_wire0	: STD_LOGIC_VECTOR (4 DOWNTO 0);
	SIGNAL sub_wire1	: STD_LOGIC ;
	SIGNAL sub_wire2	: STD_LOGIC ;
	SIGNAL sub_wire3	: STD_LOGIC ;
	SIGNAL sub_wire4	: STD_LOGIC ;
	SIGNAL sub_wire5	: STD_LOGIC ;
	SIGNAL sub_wire6	: STD_LOGIC ;
	SIGNAL sub_wire7	: STD_LOGIC_VECTOR (1 DOWNTO 0);
	SIGNAL sub_wire8_bv	: BIT_VECTOR (0 DOWNTO 0);
	SIGNAL sub_wire8	: STD_LOGIC_VECTOR (0 DOWNTO 0);



	COMPONENT altpll
	GENERIC (
		bandwidth_type		: STRING;
		clk0_divide_by		: NATURAL;
		clk0_duty_cycle		: NATURAL;
		clk0_multiply_by		: NATURAL;
		clk0_phase_shift		: STRING;
		clk1_divide_by		: NATURAL;
		clk1_duty_cycle		: NATURAL;
		clk1_multiply_by		: NATURAL;
		clk1_phase_shift		: STRING;
		clk2_divide_by		: NATURAL;
		clk2_duty_cycle		: NATURAL;
		clk2_multiply_by		: NATURAL;
		clk2_phase_shift		: STRING;
		clk3_divide_by		: NATURAL;
		clk3_duty_cycle		: NATURAL;
		clk3_multiply_by		: NATURAL;
		clk3_phase_shift		: STRING;
		compensate_clock		: STRING;
		inclk0_input_frequency		: NATURAL;
		intended_device_family		: STRING;
		lpm_hint		: STRING;
		lpm_type		: STRING;
		operation_mode		: STRING;
		pll_type		: STRING;
		port_activeclock		: STRING;
		port_areset		: STRING;
		port_clkbad0		: STRING;
		port_clkbad1		: STRING;
		port_clkloss		: STRING;
		port_clkswitch		: STRING;
		port_configupdate		: STRING;
		port_fbin		: STRING;
		port_inclk0		: STRING;
		port_inclk1		: STRING;
		port_locked		: STRING;
		port_pfdena		: STRING;
		port_phasecounterselect		: STRING;
		port_phasedone		: STRING;
		port_phasestep		: STRING;
		port_phaseupdown		: STRING;
		port_pllena		: STRING;
		port_scanaclr		: STRING;
		port_scanclk		: STRING;
		port_scanclkena		: STRING;
		port_scandata		: STRING;
		port_scandataout		: STRING;
		port_scandone		: STRING;
		port_scanread		: STRING;
		port_scanwrite		: STRING;
		port_clk0		: STRING;
		port_clk1		: STRING;
		port_clk2		: STRING;
		port_clk3		: STRING;
		port_clk4		: STRING;
		port_clk5		: STRING;
		port_clkena0		: STRING;
		port_clkena1		: STRING;
		port_clkena2		: STRING;
		port_clkena3		: STRING;
		port_clkena4		: STRING;
		port_clkena5		: STRING;
		port_extclk0		: STRING;
		port_extclk1		: STRING;
		port_extclk2		: STRING;
		port_extclk3		: STRING;
		self_reset_on_loss_lock		: STRING;
		width_clock		: NATURAL
	);
	PORT (
			areset	: IN STD_LOGIC ;
			clk	: OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
			inclk	: IN STD_LOGIC_VECTOR (1 DOWNTO 0);
			locked	: OUT STD_LOGIC 
	);
	END COMPONENT;

BEGIN
	sub_wire8_bv(0 DOWNTO 0) <= "0";
	sub_wire8    <= To_stdlogicvector(sub_wire8_bv);
	sub_wire5    <= sub_wire0(2);
	sub_wire4    <= sub_wire0(0);
	sub_wire2    <= sub_wire0(3);
	sub_wire1    <= sub_wire0(1);
	c1    <= sub_wire1;
	c3    <= sub_wire2;
	locked    <= sub_wire3;
	c0    <= sub_wire4;
	c2    <= sub_wire5;
	sub_wire6    <= inclk0;
	sub_wire7    <= sub_wire8(0 DOWNTO 0) & sub_wire6;

	altpll_component : altpll
	GENERIC MAP (
		bandwidth_type => "AUTO",
		clk0_divide_by => 1,
		clk0_duty_cycle => 50,
		clk0_multiply_by => 2,
		clk0_phase_shift => "0",
		clk1_divide_by => 1,
		clk1_duty_cycle => 50,
		clk1_multiply_by => 2,
		clk1_phase_shift => "-3000",
		clk2_divide_by => 1,
		clk2_duty_cycle => 50,
		clk2_multiply_by => 1,
		clk2_phase_shift => "0",
		clk3_divide_by => 2,
		clk3_duty_cycle => 50,
		clk3_multiply_by => 1,
		clk3_phase_shift => "0",
		compensate_clock => "CLK0",
		inclk0_input_frequency => 20000,
		intended_device_family => "Cyclone IV E",
		lpm_hint => "CBX_MODULE_PREFIX=my_altpll",
		lpm_type => "altpll",
		operation_mode => "NORMAL",
		pll_type => "AUTO",
		port_activeclock => "PORT_UNUSED",
		port_areset => "PORT_USED",
		port_clkbad0 => "PORT_UNUSED",
		port_clkbad1 => "PORT_UNUSED",
		port_clkloss => "PORT_UNUSED",
		port_clkswitch => "PORT_UNUSED",
		port_configupdate => "PORT_UNUSED",
		port_fbin => "PORT_UNUSED",
		port_inclk0 => "PORT_USED",
		port_inclk1 => "PORT_UNUSED",
		port_locked => "PORT_USED",
		port_pfdena => "PORT_UNUSED",
		port_phasecounterselect => "PORT_UNUSED",
		port_phasedone => "PORT_UNUSED",
		port_phasestep => "PORT_UNUSED",
		port_phaseupdown => "PORT_UNUSED",
		port_pllena => "PORT_UNUSED",
		port_scanaclr => "PORT_UNUSED",
		port_scanclk => "PORT_UNUSED",
		port_scanclkena => "PORT_UNUSED",
		port_scandata => "PORT_UNUSED",
		port_scandataout => "PORT_UNUSED",
		port_scandone => "PORT_UNUSED",
		port_scanread => "PORT_UNUSED",
		port_scanwrite => "PORT_UNUSED",
		port_clk0 => "PORT_USED",
		port_clk1 => "PORT_USED",
		port_clk2 => "PORT_USED",
		port_clk3 => "PORT_USED",
		port_clk4 => "PORT_UNUSED",
		port_clk5 => "PORT_UNUSED",
		port_clkena0 => "PORT_UNUSED",
		port_clkena1 => "PORT_UNUSED",
		port_clkena2 => "PORT_UNUSED",
		port_clkena3 => "PORT_UNUSED",
		port_clkena4 => "PORT_UNUSED",
		port_clkena5 => "PORT_UNUSED",
		port_extclk0 => "PORT_UNUSED",
		port_extclk1 => "PORT_UNUSED",
		port_extclk2 => "PORT_UNUSED",
		port_extclk3 => "PORT_UNUSED",
		self_reset_on_loss_lock => "OFF",
		width_clock => 5
	)
	PORT MAP (
		areset => areset,
		inclk => sub_wire7,
		clk => sub_wire0,
		locked => sub_wire3
	);

END SYN;


LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

ENTITY my_frame_buffer_15to0 IS
	PORT
	(
		data		: IN STD_LOGIC_VECTOR (11 DOWNTO 0);
		rdaddress		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		rdclock		: IN STD_LOGIC ;
		wraddress		: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
		wrclock		: IN STD_LOGIC := '1';
		wren		: IN STD_LOGIC := '0';
		q		: OUT STD_LOGIC_VECTOR (11 DOWNTO 0)
	);
END my_frame_buffer_15to0;


ARCHITECTURE SYN OF my_frame_buffer_15to0 IS

	SIGNAL sub_wire0	: STD_LOGIC_VECTOR (11 DOWNTO 0);

BEGIN
	q    <= sub_wire0(11 DOWNTO 0);

	altsyncram_component : altsyncram
	GENERIC MAP (
		address_aclr_b => "NONE",
		address_reg_b => "CLOCK1",
		clock_enable_input_a => "BYPASS",
		clock_enable_input_b => "BYPASS",
		clock_enable_output_b => "BYPASS",
		intended_device_family => "Cyclone IV E",
		lpm_type => "altsyncram",
		numwords_a => 65536,
		numwords_b => 65536,
		operation_mode => "DUAL_PORT",
		outdata_aclr_b => "NONE",
		outdata_reg_b => "UNREGISTERED",
		power_up_uninitialized => "FALSE",
		widthad_a => 16,
		widthad_b => 16,
		width_a => 12,
		width_b => 12,
		width_byteena_a => 1
	)
	PORT MAP (
		address_a => wraddress,
		clock0 => wrclock,
		data_a => data,
		wren_a => wren,
		address_b => rdaddress,
		clock1 => rdclock,
		q_b => sub_wire0
	);

END SYN;

-- Captures the pixels data of each frame coming from the OV7670 camera and 
-- Stores them in block RAM
-- The length of href controls how often pixels are captive - (2 downto 0) stores
-- one pixel every 4 cycles.
-- "line" is used to control how often data is captured. In this case every forth 
-- line

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ov7670_capture is
  Port ( pclk  : in   STD_LOGIC;
    vsync : in   STD_LOGIC;
    href  : in   STD_LOGIC;
    d     : in   STD_LOGIC_VECTOR (7 downto 0);
    addr  : out  STD_LOGIC_VECTOR (17 downto 0);
    dout  : out  STD_LOGIC_VECTOR (11 downto 0);
    we    : out  STD_LOGIC
  );
end ov7670_capture;


architecture Behavioral of ov7670_capture is


  signal d_latch      : std_logic_vector(15 downto 0) := (others => '0');
  signal address      : STD_LOGIC_VECTOR(17 downto 0) := (others => '0');
  signal line         : std_logic_vector(1 downto 0) := (others => '0');
  signal href_last    : std_logic_vector(6 downto 0) := (others => '0');
  signal we_reg       : std_logic := '0';
  signal href_hold    : std_logic := '0';
  signal latched_vsync : STD_LOGIC := '0';
  signal latched_href  : STD_LOGIC := '0';
  signal latched_d     : STD_LOGIC_VECTOR (7 downto 0) := (others => '0');
  
  
begin

  addr <= address;
  we <= we_reg;
  dout <= d_latch(15 downto 12) & d_latch(10 downto 7) & d_latch(4 downto 1); 
   
  capture_process: process(pclk)
  begin
    if rising_edge(pclk) then
    
      if we_reg = '1' then
        address <= std_logic_vector(unsigned(address)+1);
      end if;

      -- This is a bit tricky href starts a pixel transfer that takes 3 cycles
      --        Input   | state after clock tick   
      --         href   | wr_hold    d_latch           dout                we address  address_next
      -- cycle -1  x    |    xx      xxxxxxxxxxxxxxxx  xxxxxxxxxxxx  x   xxxx     xxxx
      -- cycle 0   1    |    x1      xxxxxxxxRRRRRGGG  xxxxxxxxxxxx  x   xxxx     addr
      -- cycle 1   0    |    10      RRRRRGGGGGGBBBBB  xxxxxxxxxxxx  x   addr     addr
      -- cycle 2   x    |    0x      GGGBBBBBxxxxxxxx  RRRRGGGGBBBB  1   addr     addr+1

      -- detect the rising edge on href - the start of the scan line
      if href_hold = '0' and latched_href = '1' then
        case line is
          when "00"   => line <= "01";
          when "01"   => line <= "10";
          when "10"   => line <= "11";
          when others => line <= "00";
        end case;
      end if;
      href_hold <= latched_href;
       
      -- capturing the data from the camera, 12-bit RGB
      if latched_href = '1' then
        d_latch <= d_latch( 7 downto 0) & latched_d;
      end if;
      we_reg <= '0';

      -- Is a new screen about to start (i.e. we have to restart capturing
      if latched_vsync = '1' then 
        address      <= (others => '0');
        href_last    <= (others => '0');
        line         <= (others => '0');
      else
        -- If not, set the write enable whenever we need to capture a pixel
        if href_last(2) = '1' then
          if line(1) = '1' then
            we_reg <= '1';
          end if;
          href_last <= (others => '0');
        else
          href_last <= href_last(href_last'high-1 downto 0) & latched_href;
        end if;
      end if;
    end if; 
  
  
    if falling_edge(pclk) then
      latched_d     <= d;
      latched_href  <= href;
      latched_vsync <= vsync;
    end if;
    
  end process;
  
end Behavioral;



-- Controller for the OV760 camera - transferes registers to the 
-- camera over an I2C like bus

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity ov7670_controller is
  Port ( clk    : in    STD_LOGIC;
    resend : in    STD_LOGIC;
    config_finished : out std_logic;
    sioc  : out   STD_LOGIC;
    siod  : inout STD_LOGIC;
    reset : out   STD_LOGIC;
    pwdn  : out   STD_LOGIC;
    xclk  : out   STD_LOGIC
);
end ov7670_controller;


architecture Behavioral of ov7670_controller is


  COMPONENT ov7670_registers
  PORT(
    clk      : IN std_logic;
    advance  : IN std_logic;          
    resend   : in STD_LOGIC;
    command  : OUT std_logic_vector(15 downto 0);
    finished : OUT std_logic
    );
  END COMPONENT;

  COMPONENT i2c_sender
  PORT(
    clk   : IN std_logic;
    send  : IN std_logic;
    taken : out std_logic;
    id    : IN std_logic_vector(7 downto 0);
    reg   : IN std_logic_vector(7 downto 0);
    value : IN std_logic_vector(7 downto 0);    
    siod  : INOUT std_logic;      
    sioc  : OUT std_logic
    );
  END COMPONENT;
  

  signal sys_clk  : std_logic := '0';  
  signal command  : std_logic_vector(15 downto 0);
  signal finished : std_logic := '0';
  signal taken    : std_logic := '0';
  signal send     : std_logic;
  -- device write ID; see datasheet of camera module;
  constant camera_address : std_logic_vector(7 downto 0) := x"42"; 
  
begin

  config_finished <= finished;  
  send <= not finished;
  
  Inst_i2c_sender: i2c_sender PORT MAP(
    clk   => clk,
    taken => taken,
    siod  => siod,
    sioc  => sioc,
    send  => send,
    id    => camera_address,
    reg   => command(15 downto 8),
    value => command(7 downto 0)
  );

  reset <= '1'; -- Normal mode
  pwdn  <= '0'; -- Power device up
  xclk  <= sys_clk;
  
  Inst_ov7670_registers: ov7670_registers PORT MAP(
    clk      => clk,
    advance  => taken,
    command  => command,
    finished => finished,
    resend   => resend
  );

  process(clk)
  begin
    if rising_edge(clk) then
      sys_clk <= not sys_clk;
    end if;
  end process;
  
end Behavioral;


-- register settings for the OV7670 camera (partially from OV7670.c
-- in the Linux Kernel)
--
-- this is tricky; based on my experience, using an OV7670 camera module
-- has a LOT to do with how we set/program the camera's registers;
-- it seems that the register values here get it right; thanks to the guys
-- who managed to dig this up: Mike Field, Christopher Wilson;
-- 
-- Notes:
-- 1) Regarding the WITH SELECT Statement:
--      WITH sreg(sel) SELECT
--           finished <= '1' when x"FFFF",
--                       '0' when others;
-- This means the transfer is finished the first time sreg ends up as "FFFF",  
-- i.e. Need Sequential Addresses in the below case statements 

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity ov7670_registers is
  Port ( 
    clk      : in  STD_LOGIC;
    resend   : in  STD_LOGIC;
    advance  : in  STD_LOGIC;
    command  : out  std_logic_vector(15 downto 0);
    finished : out  STD_LOGIC
  );
end ov7670_registers;


architecture Behavioral of ov7670_registers is

  signal sreg   : std_logic_vector(15 downto 0);
  signal address : std_logic_vector(7 downto 0) := (others => '0');
  
begin

  command <= sreg;
  with sreg select finished  <= '1' when x"FFFF", '0' when others;

  process(clk)
    begin
      if rising_edge(clk) then
      
        if resend = '1' then 
          address <= (others => '0');
        elsif advance = '1' then
          address <= std_logic_vector(unsigned(address)+1);
        end if;

        case address is
          when x"00" => sreg <= x"1280"; -- COM7   Reset
          when x"01" => sreg <= x"1280"; -- COM7   Reset
          when x"02" => sreg <= x"1204"; -- COM7   Size & RGB output
          when x"03" => sreg <= x"1100"; -- CLKRC  Prescaler - Fin/(1+1)
          when x"04" => sreg <= x"0C00"; -- COM3   Lots of stuff, enable scaling, all others off
          when x"05" => sreg <= x"3E00"; -- COM14  PCLK scaling off

          when x"06" => sreg <= x"8C00"; -- RGB444 Set RGB format
          when x"07" => sreg <= x"0400"; -- COM1   no CCIR601
          when x"08" => sreg <= x"4010"; -- COM15  Full 0-255 output, RGB 565
          when x"09" => sreg <= x"3a04"; -- TSLB   Set UV ordering,  do not auto-reset window
          when x"0A" => sreg <= x"1438"; -- COM9  - AGC Celling
          when x"0B" => sreg <= x"4f40"; --x"4fb3"; -- MTX1  - colour conversion matrix
          when x"0C" => sreg <= x"5034"; --x"50b3"; -- MTX2  - colour conversion matrix
          when x"0D" => sreg <= x"510C"; --x"5100"; -- MTX3  - colour conversion matrix
          when x"0E" => sreg <= x"5217"; --x"523d"; -- MTX4  - colour conversion matrix
          when x"0F" => sreg <= x"5329"; --x"53a7"; -- MTX5  - colour conversion matrix
          when x"10" => sreg <= x"5440"; --x"54e4"; -- MTX6  - colour conversion matrix
          when x"11" => sreg <= x"581e"; --x"589e"; -- MTXS  - Matrix sign and auto contrast
          when x"12" => sreg <= x"3dc0"; -- COM13 - Turn on GAMMA and UV Auto adjust
          when x"13" => sreg <= x"1100"; -- CLKRC  Prescaler - Fin/(1+1)

          when x"14" => sreg <= x"1711"; -- HSTART HREF start (high 8 bits)
          when x"15" => sreg <= x"1861"; -- HSTOP  HREF stop (high 8 bits)
          when x"16" => sreg <= x"32A4"; -- HREF   Edge offset and low 3 bits of HSTART and HSTOP

          when x"17" => sreg <= x"1903"; -- VSTART VSYNC start (high 8 bits)
          when x"18" => sreg <= x"1A7b"; -- VSTOP  VSYNC stop (high 8 bits) 
          when x"19" => sreg <= x"030a"; -- VREF   VSYNC low two bits

          when x"1A" => sreg <= x"0e61"; -- COM5(0x0E) 0x61
          when x"1B" => sreg <= x"0f4b"; -- COM6(0x0F) 0x4B 

          when x"1C" => sreg <= x"1602"; --
          when x"1D" => sreg <= x"1e37"; -- MVFP (0x1E) 0x07  -- FLIP AND MIRROR IMAGE 0x3x

          when x"1E" => sreg <= x"2102";
          when x"1F" => sreg <= x"2291";

          when x"20" => sreg <= x"2907";
          when x"21" => sreg <= x"330b";
                                
          when x"22" => sreg <= x"350b";
          when x"23" => sreg <= x"371d";
                                
          when x"24" => sreg <= x"3871";
          when x"25" => sreg <= x"392a";
                                 
          when x"26" => sreg <= x"3c78"; -- COM12 (0x3C) 0x78
          when x"27" => sreg <= x"4d40"; 
                                
          when x"28" => sreg <= x"4e20";
          when x"29" => sreg <= x"6900"; -- GFIX (0x69) 0x00
                                 
          when x"2A" => sreg <= x"6b4a";
          when x"2B" => sreg <= x"7410";
                                
          when x"2C" => sreg <= x"8d4f";
          when x"2D" => sreg <= x"8e00";
                                 
          when x"2E" => sreg <= x"8f00";
          when x"2F" => sreg <= x"9000";
                                
          when x"30" => sreg <= x"9100";
          when x"31" => sreg <= x"9600";
                                
          when x"32" => sreg <= x"9a00";
          when x"33" => sreg <= x"b084";
                                
          when x"34" => sreg <= x"b10c";
          when x"35" => sreg <= x"b20e";
                                
          when x"36" => sreg <= x"b382";
          when x"37" => sreg <= x"b80a";

          when others => sreg <= x"ffff";
        end case;
      end if;
    end process;
    
end Behavioral;



library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity RGB is
  Port ( 
    Din : in  STD_LOGIC_VECTOR (11 downto 0);  -- niveau de gris du pixels sur 8 bits
    Nblank : in  STD_LOGIC;                    -- signal indique les zone d'affichage, hors la zone d'affichage
                                               -- les trois couleurs prendre 0
    R,G,B : out  STD_LOGIC_VECTOR (7 downto 0) -- les trois couleurs sur 10 bits
  );      
end RGB;


architecture Behavioral of RGB is

begin

  R <= Din(11 downto 8) & Din(11 downto 8) when Nblank='1' else "00000000";
  G <= Din(7 downto 4)  & Din(7 downto 4)  when Nblank='1' else "00000000";
  B <= Din(3 downto 0)  & Din(3 downto 0)  when Nblank='1' else "00000000";

end Behavioral;



-- this entity is an SDRAM controller for the 64Mbyte SDRAM chips 
-- (there are two of them, U15 and U13) on DE2-115 board;
-- datasheet of these SDRAM chips is VERY important, as I refer to it
-- in some comments, and you can find its pdf in the Terasic's CD
-- that comes with the board;
-- the datasheet is for part number: IS45S16320B; you can download it from here:
-- http://www.issi.com/WW/pdf/42S16320B-86400B.pdf
-- Notes:
-- 1) this code is a direct translation from Verlig to VHDL; original Verilog is here:
--    http://whoyouvotefor.info/altera_sdram.shtml
-- 2) this coding style is not good; and it should be cleaned up;
--    for instance, we should not have if statements with missing else branches
--    or signals left unassigned in ALL possible situations captured by
--    partial if or case statements;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_unsigned.all;


entity sdram_controller is
  Port (
    clk_i: in  STD_LOGIC; -- clk_100
    dram_clk_i: in  STD_LOGIC; -- clk_100_3ns
    rst_i: in  STD_LOGIC;
    dll_locked: in  STD_LOGIC;
    -- all ddr signals
    dram_addr: out  STD_LOGIC_vector(12 downto 0);
    dram_bank: out  STD_LOGIC_vector(1 downto 0);
    dram_cas_n: out  STD_LOGIC;
    dram_cke: out  STD_LOGIC;
    dram_clk: out  STD_LOGIC;
    dram_cs_n: out  STD_LOGIC;
    dram_dq: inout  STD_LOGIC_vector(15 downto 0);
    dram_ldqm: out  STD_LOGIC;
    dram_udqm: out  STD_LOGIC;
    dram_ras_n: out  STD_LOGIC;
    dram_we_n: out  STD_LOGIC;
	 oe_r_r: buffer std_logic;
    -- wishbone bus
    addr_i: in  STD_LOGIC_vector(24 downto 0); 
    dat_i: in  STD_LOGIC_vector(31 downto 0);
    dat_o: out  STD_LOGIC_vector(31 downto 0);
    we_i: in  STD_LOGIC;
    ack_o: out  STD_LOGIC;
    stb_i: in  STD_LOGIC;
    cyc_i: in  STD_LOGIC
  );
end sdram_controller;


architecture my_behavioral of sdram_controller is

  -- row width 13
  -- column width 10
  -- bank width 2
  -- user address is specified as {bank,row,column}

  -- now, look at page 24 of datasheet of SDRAM chips;
  -- we see address[2:0] is the Burst Length (BL), which we'll set here as 2; so, address[2:0] must be "001"
  -- also, address[3] is Burst Type; we set it to Sequential; so, address[3] must be '0';
  -- also, address[6:4] is CAS Latency, which we set to 3 here; so, address[6:4] must be "011";
  -- MODE_REGISTER is defined as: BA1 BA0 A12 A11 A10 A9 A8 A7 A6 A5 A4 A3 A2 A1 A0
  -- with BA1 BA0 A12 A11 A10 being reserved;
  -- so we use A2 A1 A0 = "001" to BL=2; A3 = '0'; A6 A5 A4 = "011" to CAS=3
  -- Note: I am defining MODE_REGISTER with just 13 bits instead of 15 bits; discard BA1 BA0;
  constant MODE_REGISTER : std_logic_vector(12 downto 0) := "0000000110001";
    
  constant INIT_IDLE          : std_logic_vector(2 downto 0) := "000";
  constant INIT_WAIT_200us    : std_logic_vector(2 downto 0) := "001";
  constant INIT_INIT_PRE      : std_logic_vector(2 downto 0) := "010";
  constant INIT_WAIT_PRE      : std_logic_vector(2 downto 0) := "011";
  constant INIT_MODE_REG      : std_logic_vector(2 downto 0) := "100";
  constant INIT_WAIT_MODE_REG : std_logic_vector(2 downto 0) := "101";
  constant INIT_DONE_ST       : std_logic_vector(2 downto 0) := "110";

  constant IDLE_ST         : std_logic_vector(3 downto 0) := "0000";
  constant REFRESH_ST      : std_logic_vector(3 downto 0) := "0001";
  constant REFRESH_WAIT_ST : std_logic_vector(3 downto 0) := "0010";
  constant ACT_ST          : std_logic_vector(3 downto 0) := "0011";
  constant WAIT_ACT_ST     : std_logic_vector(3 downto 0) := "0100";
  constant WRITE0_ST       : std_logic_vector(3 downto 0) := "0101";
  constant WRITE1_ST       : std_logic_vector(3 downto 0) := "0110";
  constant WRITE_PRE_ST    : std_logic_vector(3 downto 0) := "0111";
  constant READ0_ST        : std_logic_vector(3 downto 0) := "1000";
  constant READ1_ST        : std_logic_vector(3 downto 0) := "1001";
  constant READ2_ST        : std_logic_vector(3 downto 0) := "1010";
  constant READ3_ST        : std_logic_vector(3 downto 0) := "1011";
  constant READ4_ST        : std_logic_vector(3 downto 0) := "1100";
  constant READ_PRE_ST     : std_logic_vector(3 downto 0) := "1101";
  constant PRE_ST          : std_logic_vector(3 downto 0) := "1110";
  constant WAIT_PRE_ST     : std_logic_vector(3 downto 0) := "1111";


  -- if 100 MHz, then, period is T_CLK = 10 ns 
  -- 7 cycles == time to wait after refresh 70ns
  -- also time to wait between two ACT commands; I'll make it 100ns though;
  constant TRC_CNTR_VALUE : std_logic_vector(3 downto 0) := std_logic_vector(to_unsigned(10, 4));
  -- need 8192=2^13 refreshes for every 64_000_000 ns 
  -- (every 64ms, see page 7 of datasheet of ISSI SDRAM chips on DE2-115)
  -- so the # of cycles between refreshes is 64000000 / 8192 / 10 = 781.25; I'll make 780 though;
  constant RFSH_INT_CNTR_VALUE : std_logic_vector(24 downto 0) := std_logic_vector(to_unsigned(780, 25));
  -- ras to cas delay 20 ns; that's about 2 T_CLK
  -- will also be used for tRP and tRSC
  constant TRCD_CNTR_VALUE : std_logic_vector(2 downto 0) := std_logic_vector(to_unsigned(2, 3));
  -- TODO: datasheet of SDRAM chips (page 20) says that 
  -- A 100us delay is required prior to issuing any command other than a COMMAND INHIBIT or a NOP.
  -- 20000 cycles to make up 200 us instead of 100 us
  constant WAIT_200us_CNTR_VALUE : std_logic_vector(15 downto 0) := std_logic_vector(to_unsigned(20000, 16)); 

  signal address_r: std_logic_vector(24 downto 0); 
  
  signal dram_addr_r: std_logic_vector(12 downto 0); 
  signal dram_bank_r: std_logic_vector(1 downto 0);
  signal dram_dq_r: std_logic_vector(15 downto 0); 
  signal dram_cas_n_r: std_logic;
  signal dram_ras_n_r: std_logic;
  signal dram_we_n_r: std_logic;

  signal dat_o_r: std_logic_vector(31 downto 0) := (others => '0');
  signal ack_o_r: std_logic := '0';
  signal dat_i_r: std_logic_vector(31 downto 0);
  signal we_i_r: std_logic := '0';
  signal stb_i_r: std_logic;
  signal oe_r: std_logic := '0';

  signal current_state: std_logic_vector(3 downto 0) := IDLE_ST;
  signal next_state: std_logic_vector(3 downto 0) := IDLE_ST;
  signal current_init_state: std_logic_vector(2 downto 0) := INIT_IDLE;
  signal next_init_state: std_logic_vector(2 downto 0) := INIT_IDLE;
     
  signal init_done: std_logic := '0';
  signal init_pre_cntr: std_logic_vector(3 downto 0) := (others => '0');
  signal trc_cntr: std_logic_vector(3 downto 0) := (others => '0');
  signal rfsh_int_cntr: std_logic_vector(24 downto 0) := (others => '0');      
  signal trcd_cntr: std_logic_vector(2 downto 0) := (others => '0');
  signal wait_200us_cntr: std_logic_vector(15 downto 0) := (others => '0');
  signal do_refresh: std_logic;

begin

  dram_addr <= dram_addr_r;
  dram_bank <= dram_bank_r;
  dram_cas_n <= dram_cas_n_r;
  dram_ras_n <= dram_ras_n_r;
  dram_we_n <= dram_we_n_r;
  dram_dq <= dram_dq_r when oe_r = '1' else (others => 'Z');    

  dat_o <= dat_o_r;
  ack_o <= ack_o_r;
    
  dram_cke <= '1'; -- dll_locked
  dram_cs_n <= not dll_locked; -- chip select is always on in normal op
  dram_clk <= dram_clk_i;
  dram_ldqm <= '0'; -- don't do byte masking
  dram_udqm <= '0'; -- don't do byte masking
  
  oe_r_r <= oe_r; 
    
  process (clk_i)
  begin
    if rising_edge (clk_i) then
      if (stb_i_r = '1' and current_state = ACT_ST) then
        stb_i_r <= '0';
      elsif (stb_i = '1' and cyc_i = '1') then
        address_r <= addr_i;
        dat_i_r <= dat_i;
        we_i_r <= we_i; -- pick whatever value we_i has;
        stb_i_r <= stb_i;
      end if;
    end if;
  end process;
   
  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        wait_200us_cntr <= (others => '0');
      elsif (current_init_state = INIT_IDLE) then
        wait_200us_cntr <= WAIT_200us_CNTR_VALUE;
      else 
        wait_200us_cntr <= wait_200us_cntr - 1;
      end if;
    end if;
  end process;
    
  -- control the interval between refreshes
  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        rfsh_int_cntr <= (others => '0'); -- immediately initiate new refresh on reset
      elsif (current_state = REFRESH_WAIT_ST) then
        do_refresh <= '0';
        rfsh_int_cntr <= RFSH_INT_CNTR_VALUE;
      elsif (rfsh_int_cntr = "0000000000000000000000000") then
        do_refresh <= '1';
      else 
        rfsh_int_cntr <= rfsh_int_cntr - 1; 
      end if;
    end if;
  end process;     

  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        trc_cntr <= "0000";
      elsif (current_state = PRE_ST or current_state = REFRESH_ST) then
        trc_cntr <= TRC_CNTR_VALUE;
      else 
        trc_cntr <= trc_cntr - 1; 
      end if;
    end if;  
  end process;  

  -- counter to control the activate
  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        trcd_cntr <= "000";
      elsif (current_state = ACT_ST or current_init_state = INIT_INIT_PRE 
        or current_init_state = INIT_MODE_REG) then
        trcd_cntr <= TRCD_CNTR_VALUE;
      else 
        trcd_cntr <= trcd_cntr - 1;
      end if;
    end if;
  end process;

  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        init_pre_cntr <= "0000";
      elsif (current_init_state = INIT_INIT_PRE) then
        init_pre_cntr <= init_pre_cntr + 1;
      end if;
    end if;
  end process;

  process (clk_i)
  begin
    if rising_edge (clk_i) then
      if (current_init_state = INIT_DONE_ST) then
        init_done <= '1';
      end if;
    end if;
  end process;

  -- state change
  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        current_init_state <= INIT_IDLE;
      else      
        current_init_state <= next_init_state;
      end if;
    end if;
  end process;

  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        current_state <= IDLE_ST;
      else 
        current_state <= next_state;
      end if;
    end if;
  end process; 

  -- initialization is fairly easy on this chip: wait 200us then issue
  -- 8 precharges before setting the mode register
  process (current_init_state)
  begin
    case current_init_state is  
      when INIT_IDLE =>
        if (init_done = '0') then 
          next_init_state <= INIT_WAIT_200us;
        else 
          next_init_state <= INIT_IDLE;
        end if;
        
      when INIT_WAIT_200us =>
        if (wait_200us_cntr = "0000000000000000") then 
          next_init_state <= INIT_INIT_PRE;
        else 
          next_init_state <= INIT_WAIT_200us;
        end if;
        
      when INIT_INIT_PRE =>
        next_init_state <= INIT_WAIT_PRE;

      when INIT_WAIT_PRE =>
        if (trcd_cntr = "000") then -- this is tRP
          if (init_pre_cntr = "1000") then
            next_init_state <= INIT_MODE_REG;
          else
            next_init_state <= INIT_INIT_PRE;
          end if;
        else 
          next_init_state <= INIT_WAIT_PRE;
        end if;

      when INIT_MODE_REG =>
        next_init_state <= INIT_WAIT_MODE_REG;
        
      when INIT_WAIT_MODE_REG =>
        if (trcd_cntr = "000") then -- tRSC
          next_init_state <= INIT_DONE_ST;
        else 
          next_init_state <= INIT_WAIT_MODE_REG;
        end if;
      
      when INIT_DONE_ST =>
        next_init_state <= INIT_IDLE;

      when others =>
        next_init_state <= INIT_IDLE;      
    end case;
  end process;
   
  -- this is the main controller logic:
  process (current_state)
  begin
    case current_state is  
      when IDLE_ST =>
        if (init_done = '0') then
          next_state <= IDLE_ST;
        elsif (do_refresh = '1') then 
          next_state <= REFRESH_ST;
        elsif (stb_i_r = '1') then 
          next_state <= ACT_ST;
        else   
          next_state <= IDLE_ST;
        end if;
        
      when REFRESH_ST => 
        next_state <= REFRESH_WAIT_ST;

      when REFRESH_WAIT_ST =>
        if (trc_cntr = "0000") then 
          next_state <= IDLE_ST;
        else 
          next_state <= REFRESH_WAIT_ST;
        end if;
        
      when ACT_ST => 
        next_state <= WAIT_ACT_ST;
        
      when WAIT_ACT_ST =>
        if (trcd_cntr = "000") then
          if (we_i_r = '1') then 
            next_state <= WRITE0_ST;
          else  
            next_state <= READ0_ST;
          end if;
        else
          next_state <= WAIT_ACT_ST;
        end if;
        
      when WRITE0_ST => 
        next_state <= WRITE1_ST;

      when WRITE1_ST =>
        next_state <= WRITE_PRE_ST;
        
      when WRITE_PRE_ST =>
        next_state <= PRE_ST;
        
      when READ0_ST =>  
        next_state <= READ1_ST;

      when READ1_ST => 
        next_state <= READ2_ST;
        
      when READ2_ST => 
        next_state <= READ3_ST;

      when READ3_ST =>
        next_state <= READ4_ST;

      when READ4_ST =>  
        next_state <= READ_PRE_ST;

      when READ_PRE_ST => 
        next_state <= PRE_ST;
        
      when PRE_ST => 
        next_state <= WAIT_PRE_ST;
        
      when WAIT_PRE_ST =>
        -- if the next command was not another row activate in the same bank
        -- we could wait tRCD only; for simplicity but at the detriment of
        -- efficiency we always wait tRC
        if (trc_cntr = "0000") then 
          next_state <= IDLE_ST;
        else 
          next_state <= WAIT_PRE_ST;
        end if;

      when others => 
        next_state <= IDLE_ST;        
    end case;
  end process; 

  -- ack_o signal
  process (clk_i)
  begin
    if rising_edge (clk_i) then
      if (current_state = READ_PRE_ST or current_state = WRITE_PRE_ST) then
        ack_o_r <= '1';
      elsif (current_state = WAIT_PRE_ST) then
        ack_o_r <= '0';
      end if;
    end if;
  end process;  
   
  -- data
  process (clk_i, rst_i)
  begin
    if rising_edge (clk_i) then
      if (rst_i = '1') then
        dat_o_r <= (others => '0'); 
        dram_dq_r <= (others => '0');
        oe_r <= '0';
      elsif (current_state = WRITE0_ST) then
        dram_dq_r <= dat_i_r(31 downto 16);
        oe_r <= '1';
      elsif (current_state = WRITE1_ST) then
        dram_dq_r <= dat_i_r(15 downto 0);
        oe_r <= '1';
      elsif (current_state = READ4_ST) then
        -- we should actually be reading this on READ3, but
        -- because of delay the data comes a cycle later...
        dat_o_r(31 downto 16) <= dram_dq;
        dram_dq_r <= (others => 'Z');
        oe_r <= '0';
      elsif (current_state = READ_PRE_ST) then
        dat_o_r(15 downto 0) <= dram_dq; 
        dram_dq_r <= (others => 'Z');
        oe_r <= '0';
      else 
        dram_dq_r <= (others => 'Z');
        oe_r <= '0';
      end if;
    end if;
  end process;

  -- address
  process (clk_i)
  begin
    if rising_edge (clk_i) then
      if (current_init_state = INIT_MODE_REG) then
        dram_addr_r <= MODE_REGISTER;
      elsif (current_init_state = INIT_INIT_PRE) then
        -- from page 6 of datasheet of SDRAM chips on DE2-115 board: 
        -- A10 is sampled during a PRECHARGE command to
        -- determine if all banks are to be precharged (A10 HIGH) 
        -- or bank selected by BA0, BA1 (LOW).
        dram_addr_r <= "0010000000000"; -- A[10] = '1' to precharge all
      elsif (current_state = ACT_ST) then
        dram_addr_r <= address_r(22 downto 10);
        dram_bank_r <= address_r(24 downto 23);
      elsif (current_state = WRITE0_ST or current_state = READ0_ST) then
        -- enter column with bit A10 set to 1 indicating auto precharge;
        dram_addr_r <= "001" & address_r(9 downto 0);
        dram_bank_r <= address_r(24 downto 23);
      else 
        dram_addr_r <= (others => '0');
        dram_bank_r <= "00";
      end if;
    end if;
  end process;

  -- commands
  process (clk_i)
  begin
    if rising_edge (clk_i) then     
      if (current_init_state = INIT_INIT_PRE 
        or current_init_state = INIT_MODE_REG 
        or current_state = REFRESH_ST 
        or current_state = ACT_ST) then
        dram_ras_n_r <= '0';
      else 
        dram_ras_n_r <= '1';
      end if;
      
      if (current_state = READ0_ST 
        or current_state = WRITE0_ST 
        or current_state = REFRESH_ST 
        or current_init_state = INIT_MODE_REG) then
        dram_cas_n_r <= '0';
      else 
        dram_cas_n_r <= '1';
      end if;
       
      if (current_init_state = INIT_INIT_PRE 
        or current_state = WRITE0_ST 
        or current_init_state = INIT_MODE_REG) then
        dram_we_n_r <= '0';
      else
        dram_we_n_r <= '1';
      end if;
    end if;
  end process;
  
end my_behavioral;



-- this entity is the middle-man between the SDRAM controller
-- (which is hooked to the SDRAM chip on board) and my application;
-- used here to write into sdram the frame from frame buffer 1;
-- also, used to read from sdram and place the frame into frame buffer 2;
-- the led done is asserted to signal that all pixels have been written or read;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use ieee.std_logic_unsigned.all;

entity sdram_rw_and_image_processing is
  Port ( 
    -- connections to sdram controller;
    clk_i : in  STD_LOGIC; -- 25 MHz
    rst_i : in  STD_LOGIC;
	 
    addr_i : out  STD_LOGIC_vector(24 downto 0); 
    dat_i : out  STD_LOGIC_vector(31 downto 0);
    dat_o : in  STD_LOGIC_vector(31 downto 0);
    we_i : out  STD_LOGIC;
    ack_o : in  STD_LOGIC;
    stb_i : out  STD_LOGIC;
    cyc_i : out  STD_LOGIC;
    -- connections to frame buffer 2 for which we need to
    -- generate addresses and pass image data from SDRAM;
    addr_buf2 : OUT STD_LOGIC_VECTOR (17 downto 0);
    dout_buf2 : buffer std_logic_vector(11 downto 0);
    we_buf2 : OUT std_logic;
    -- connections from frame buffer 1 from where we 
    -- take snapshot; we of buf 1 is controlled by ov7670_capture
    -- here we only read from buffer 1;
    addr_buf1 : OUT STD_LOGIC_VECTOR (17 downto 0);
    din_buf1 : IN std_logic_vector(11 downto 0); 
	 din_imagepro: in std_logic_vector(11 downto 0);
	 
    -- rw controls
    take_snapshot1 : in  STD_LOGIC; -- store to SDRAM;
	 take_snapshot2 : in  STD_LOGIC;
	 take_snapshot1_auto : buffer  STD_LOGIC; -- store to SDRAM;
	 take_snapshot2_auto : buffer  STD_LOGIC;
	 btn_take_snapshot :in std_logic;
    display_snapshot1 : in  STD_LOGIC;
    display_snapshot2 : in  STD_LOGIC; -- read/fetch from SDRAM;
	 display_sub : in  STD_LOGIC;
	 display_threshold : in  STD_LOGIC;
	 display_median : in  STD_LOGIC;
	 display_all : in  STD_LOGIC;
	 display_location : in std_logic;
    led_done : out  STD_LOGIC;
	 led_sub_done: out  STD_LOGIC;
	 numberToDisplay1:out std_logic_vector(17 downto 0);
	 numberToDisplay2:out std_logic_vector(17 downto 0);
	 numberToDisplay3:out std_logic_vector(17 downto 0);
	 numberToDisplay4:out std_logic_vector(17 downto 0);
	 sw2_take_snapshot1 		: in std_logic;
	 sw3_take_snapshot2		: in std_logic;
	 sw4_display_snapshot1	: in std_logic;
	 sw5_display_snapshot2	: in std_logic;
	 sw6_auto_enable			: in std_logic;
	 sw7_sub_display			: in std_logic;
	 sw8_sub_activate			: in std_logic;
	 sw9_thresh_activate		: in std_logic;
	 sw10_thresh_display		: in std_logic;
	 sw11_median_activate	: in std_logic;
	 sw12_median_display		: in std_logic;
	 sw13_all_activate		: in std_logic;
	 sw14_all_disp				: in std_logic;
	 sw15_location_disp	   : in std_logic;
	 frameOne :buffer std_logic;
	 frameTwo : buffer std_logic;
	 cmd_engine_x: buffer integer;
	 cmd_engine_y: buffer integer;
	 trigger: buffer std_logic;
	 ack_trigger: in std_logic
  );
end sdram_rw_and_image_processing;


architecture my_behavioral of sdram_rw_and_image_processing is

  constant START_WRITE_ST     : std_logic_vector(5 downto 0) := "000000"; --0
  constant WRITE_ST           : std_logic_vector(5 downto 0) := "000001"; --1
  constant WAIT_WRITE_ACK_ST  : std_logic_vector(5 downto 0) := "000010"; --2
  constant READ_ST            : std_logic_vector(5 downto 0) := "000011"; --3
  constant WAIT_READ_ACK_ST   : std_logic_vector(5 downto 0) := "000100"; --4
  constant WRITE_WAIT_ST      : std_logic_vector(5 downto 0) := "000101"; --5
  constant START_READ_ST      : std_logic_vector(5 downto 0) := "000110"; --6
  constant READ_WAIT_ST       : std_logic_vector(5 downto 0) := "000111"; --7
  constant DONE_ST            : std_logic_vector(5 downto 0) := "001000"; --8
  constant IDLE_ST            : std_logic_vector(5 downto 0) := "001001"; --9
  
  constant INIT_sub_st					: std_logic_vector(5 downto 0) := "001010"; --10
  constant Start_Read_Sub_st  		: std_logic_vector(5 downto 0) := "001011"; --11
  constant Read_pixel_st  				: std_logic_vector(5 downto 0) := "001100"; --12
  constant Wait_Read_Ack_pixel_st	: std_logic_vector(5 downto 0) := "001101"; --13
  constant Read_Wait_pixel_st			: std_logic_vector(5 downto 0) := "001110"; --14
  constant sub_and_avg_st 				: std_logic_vector(5 downto 0) := "001111"; --15
  constant Start_Write_Sub_st    	: std_logic_vector(5 downto 0) := "010000"; --16
  constant write_Sub_st   			   : std_logic_vector(5 downto 0) := "010001"; --17
  constant Wait_Write_Ack_Sub_st   	: std_logic_vector(5 downto 0) := "010010"; --18
  constant write_wait_Sub_and_avg_st: std_logic_vector(5 downto 0) := "010011"; --19
  constant done_Sub_and_avg_st      : std_logic_vector(5 downto 0) := "010100"; --20
  
  constant start_read_threshold_st	  : std_logic_vector(5 downto 0) := "010101"; --21
  constant read_threshold_st 		     : std_logic_vector(5 downto 0) := "010110"; --22
  constant wait_read_ack_threshold_st : std_logic_vector(5 downto 0) := "010111"; --23
  constant read_wait_threshold_st 	  : std_logic_vector(5 downto 0) := "011000"; --24
  constant threshold_st     			  : std_logic_vector(5 downto 0) := "011001"; --25
  constant start_write_threshold_st   : std_logic_vector(5 downto 0) := "011010"; --26
  constant write_threshold_st    	  : std_logic_vector(5 downto 0) := "011011"; --27
  constant wait_write_threshold_ack_st: std_logic_vector(5 downto 0) := "011100"; --28
  constant write_wait_threshold_st    : std_logic_vector(5 downto 0) := "011101"; --29
  constant done_threshold_st          : std_logic_vector(5 downto 0) := "011110"; --30

  constant check_pixel_location_median_st	  : std_logic_vector(5 downto 0) := "011111"; --31  
  constant start_read_median_st	  : std_logic_vector(5 downto 0) := "100000"; --32
  constant read_median_st 		     : std_logic_vector(5 downto 0) := "100001"; --33
  constant wait_read_ack_median_st : std_logic_vector(5 downto 0) := "100010"; --34
  constant read_wait_median_st 	  : std_logic_vector(5 downto 0) := "100011"; --35
  constant find_median_st     			  : std_logic_vector(5 downto 0) := "100100"; --36
  constant start_write_median_st   : std_logic_vector(5 downto 0) := "100101"; --37
  constant write_median_st    	  : std_logic_vector(5 downto 0) := "100110"; --38
  constant address_change_st: std_logic_vector(5 downto 0) 			 := "100111"; --39
  constant write_location_median_st: std_logic_vector(5 downto 0) := "101000"; --40
  constant wait_write_ack_median_st: std_logic_vector(5 downto 0) := "101001"; --41
  constant write_wait_median_st    : std_logic_vector(5 downto 0) := "101010"; --42
  constant done_median_st          : std_logic_vector(5 downto 0) := "101011"; --43
  
  constant calculate_location_st     			  : std_logic_vector(5 downto 0) := "101100"; --44
  constant start_write_location_st   : std_logic_vector(5 downto 0) := "101101"; --45
  constant write_location_st    	  : std_logic_vector(5 downto 0) := "101110"; --46
  constant wait_write_ack_location_st: std_logic_vector(5 downto 0) := "101111"; --47
  constant write_wait_location_st    : std_logic_vector(5 downto 0) := "110000"; --48
  constant done_location_st          : std_logic_vector(5 downto 0) := "110001"; --49
  
  -- we need to read 320x240 = 76800 words; we use 1 word in SDRAM per pixel
  -- even though right now pixel data is only 12 bits of useful info; 
  -- this is just in case we'll use 24 bits per pixel in the future;
  constant NUM_PIXELS : std_logic_vector(17 downto 0) := std_logic_vector(to_unsigned(76799, 18)); 
  --for average
  constant num_pixels_avg : std_logic_vector(24 downto 0) := std_logic_vector(to_unsigned(76799, 25)); 
  -- signal that storing or fetching one frame is done;
  signal led_done_r: std_logic := '0';
  signal led_sub_done_r: std_logic := '0';
  

  -- coming from buffer 1, for storage of snapshot into SDRAM;
  signal addr_buf1_r : STD_LOGIC_VECTOR(17 downto 0) := (others => '0');
  signal din_buf1_r : std_logic_vector(11 downto 0);
  
  signal din_imagepro_r : std_logic_vector(11 downto 0);
  signal dataBufferout_r: std_logic_vector(11 downto 0);
  
  -- going to buffer 2, for display;
  signal addr_buf2_r : STD_LOGIC_VECTOR(17 downto 0) := (others => '0');
  signal dout_buf2_r : std_logic_vector(11 downto 0) := (others => '0');
  signal we_buf2_r : std_logic := '0'; 

  -- redundant counter; keeps track of num of pixels written/read from sdram;
  signal rw_cntr: std_logic_vector(17 downto 0) := (others => '0');
  -- state of the FSM that implements the read or write steps;
  signal state: std_logic_vector(5 downto 0) := IDLE_ST;

  -- address bus to sdram_controller;
  signal addr_i_r: std_logic_vector(24 downto 0) := (others => '0'); 
  signal dat_i_r: std_logic_vector(31 downto 0);
  signal dat_o_r: std_logic_vector(31 downto 0);
  signal we_i_r: std_logic := '0';
  signal stb_i_r: std_logic := '0';
  signal cyc_i_r: std_logic := '0';
  signal frameOneWrite :std_logic := '0';
  signal frameTwoWrite:std_logic := '0';
  signal frame_number:integer := 1;
  signal sub_result: std_logic_vector(11 downto 0);
  signal pixel_temp1: std_logic_vector(11 downto 0) := "000000000000";
  signal pixel_temp2: std_logic_vector(11 downto 0) := "000000000000";  
  signal Sum_red : std_logic_vector(24 downto 0) := (others => '0');
  signal Sum_green : std_logic_vector(24 downto 0) := (others => '0');
  signal Sum_blue : std_logic_vector(24 downto 0) := (others => '0');
  signal Avg_red : std_logic_vector(24 downto 0) := (others => '0');
  signal Avg_green : std_logic_vector(24 downto 0) := (others => '0');
  signal Avg_blue : std_logic_vector(24 downto 0) := (others => '0');
  signal Red_pixel : std_logic_vector(7 downto 0);
  signal green_pixel : std_logic_vector(7 downto 0);
  signal blue_pixel : std_logic_vector(7 downto 0);
  signal thresh_result : std_logic_vector(11 downto 0);  
  signal flag_sub_done : std_logic := '0';
  signal flag_thresh_done : std_logic := '0';
  signal flag_median_done : std_logic := '0';
  signal Flag_location_done : std_logic := '0'; 
  signal temp1 :std_logic_vector(17 downto 0) := (others => '0');
  signal temp2 :std_logic_vector(17 downto 0) := (others => '0');
  signal temp3 :std_logic_vector(17 downto 0) := (others => '0');
  signal enable_all_sub : std_logic := '0';
  signal enable_all_sub_next : std_logic := '0';
  signal enable_all_thresh : std_logic := '0';
  signal enable_all_thresh_next : std_logic := '0';
  signal enable_all_median : std_logic := '0';
  signal enable_all_median_next : std_logic := '0';
  signal enable_all_display : std_logic := '0';
  signal enable_sw13 : std_logic := '1';
  
  --signal 2frames arrived
  signal ledRed3: std_logic;
  signal median_value	 : std_logic;
  signal left_pixel: std_logic_vector(17 downto 0) := "010010101111111111";
  signal right_pixel: std_logic_vector(17 downto 0) := (others => '0');  
  signal up_pixel: std_logic_vector(17 downto 0) := "010010100110000000"; 
  signal down_pixel: std_logic_vector(17 downto 0) := (others => '0');
  signal	address_pixel :integer;
  signal r_cnt: integer := 0;
  signal timer_enable_frame1: std_logic := '0';
  signal timer_enable_frame2: std_logic := '0';
  
begin
  frameOne <= frameOneWrite;
  frameTwo <= frameTwoWrite;
  numberToDisplay1 <=  "00000000000000000" & timer_enable_frame2; 	--left up
  numberToDisplay2 <= temp1;  --left down
  numberToDisplay3 <=  "000000000000" & state; 						--right up
  numberToDisplay4 <= temp2; --right down
  dat_o_r <= dat_o;
  addr_i <= addr_i_r;
  dat_i <= dat_i_r; -- should not be really necessary;
  stb_i <= stb_i_r;
  cyc_i <= cyc_i_r;
  we_i <= we_i_r;
  ledRed3 <= '1' when (frameOne='1' and frameTwo='1') and (led_sub_done_r='0') else '0';

  -- writes into sdram: buffer 1 --> sdram;
  addr_buf1 <= addr_buf1_r; -- needed because I increment addr_buf1_r;
  din_buf1_r <= din_buf1;
  --grayscale write
  din_imagepro_r <= din_imagepro;

  -- read backs: sdram --> buffer 2;
  addr_buf2 <= addr_buf2_r;
  dout_buf2 <= dout_buf2_r;
  we_buf2 <= we_buf2_r; 
  
  led_done <= led_done_r;
  led_sub_done <= led_sub_done_r;

  process (clk_i)
		variable median49counter:integer;
		variable x : integer;
		variable x_left : integer;
		variable x_right : integer;
		variable y : integer;
		variable	location_pixel :integer := 38560;
		variable	mark_counter :integer;
		variable ones_counter :integer;
		
  begin
    if rising_edge (clk_i) then
	    if (rst_i = '1') then
			  state <= IDLE_ST;
			  led_done_r <= '0';
			  led_sub_done_r <= '0';
			  we_buf2_r <= '0';
		else
        case state is
        
          -- part 1: sequence of states related to writes;
          -- state START_WRITE_ST is visited once only for each frame;
          when START_WRITE_ST =>
            state <= WRITE_ST;
				if sw3_take_snapshot2='1' or take_snapshot2_auto = '1' then
				   addr_i_r <= "0000000000000000000000010"; -- take 2
					frameTwoWrite <= '1';
				elsif sw2_take_snapshot1='1' or take_snapshot1_auto = '1' then
					addr_i_r <= (others => '0'); -- take 1
					frameOneWrite <= '1';
				end if;
            rw_cntr <= (others => '0');
            we_i_r <= '1'; -- stays like that during writes;
            addr_buf1_r <= (others => '0');
          
          -- each pixel data writing into sdram goes tru the sequence of 3 states:
          -- WRITE_ST --> WAIT_WRITE_ACK_ST --> WRITE_WAIT_ST
          when WRITE_ST =>
            state <= WAIT_WRITE_ACK_ST;
            stb_i_r <= '1';
            cyc_i_r <= '1';
            we_i_r <= '1';
            -- now, this is a bit tricky: pick up data coming from buffer 1 here;
            -- data should be stable already, because I changed the address in the previous
            -- cycle inside state WRITE_WAIT_ST, but on falling edge of clk_i, from within
            -- a different process;
            --dat_i_r <= ("00000000000000000000" & "000000001111"); -- blue debug; 
				---- dat_i_r <= ("00000000000000000000" & din_buf1_r); -- pass this to be written to sdram;
            dat_i_r <= ("00000000000000000000" & din_imagepro_r);
				-- increment address for buf1; very important to do this here instead of
            -- in WRITE_WAIT_ST state; otherwise, we get a pixel lag that will shift the image
            -- to the right;
            addr_buf1_r <= addr_buf1_r + 1;

          when WAIT_WRITE_ACK_ST =>
            if (ack_o = '1') then
              state <= WRITE_WAIT_ST;
              stb_i_r <= '0';
              cyc_i_r <= '0';
            end if;
            
          when WRITE_WAIT_ST =>
            if (rw_cntr < NUM_PIXELS) then
              state <= WRITE_ST;
              rw_cntr <= rw_cntr + 1; -- keep track of how many times we write into sdram;
				  addr_i_r <= addr_i_r + 14;
           -- increment address for sdram controller;
            else 
					
              state <= DONE_ST;
            end if;                  
            
          -- part 2: states below related to read backs;
          -- state START_READ_ST is visited once only for each frame;
          when START_READ_ST => 
            if sw5_display_snapshot2='1' then
				   addr_i_r <= "0000000000000000000000010"; -- display2
				elsif sw7_sub_display='1' then
					addr_i_r <= "0000000000000000000000100"; --sub				
				elsif sw10_thresh_display='1' then
					addr_i_r <= "0000000000000000000000110";
				elsif sw12_median_display='1' then
					addr_i_r <= "0000000000000000000001000";
				elsif sw14_all_disp='1' then
					addr_i_r <= "0000000000000000000001000";	
				elsif sw15_location_disp = '1' then
					addr_i_r <= "0000000000000000000001010";
				else
					addr_i_r <= (others => '0'); --display1
				end if;
            rw_cntr <= (others => '0');
            we_i_r <= '0'; -- stays like that during read backs;
            addr_buf2_r <= (others => '0');
            dout_buf2_r <= "110000000011"; -- Yellow;
            we_buf2_r <= '1';
            state <= READ_ST;
              
          -- each pixel data reading process goes tru the sequence of 3 states:
          -- READ_ST --> WAIT_READ_ACK_ST --> READ_WAIT_ST
          -- which means three clock periods of 25 MHz; note that sdram
          -- controller operates with clk_100 that is much faster;
          when READ_ST => -- tell sdram_controller we want to read from addr_i_r
            stb_i_r <= '1';
            cyc_i_r <= '1';
            we_i_r <= '0';
            state <= WAIT_READ_ACK_ST;
          
          when WAIT_READ_ACK_ST => -- wait for controller which should assert ack_o 
            -- here we "stall" in this state until ack_o is asserted;
            if (ack_o = '1') then
              stb_i_r <= '0';
              cyc_i_r <= '0';
              dout_buf2_r <= dat_o_r(11 downto 0); -- what comes from sdram_controller is sent to buffer 2;
              state <= READ_WAIT_ST;
            end if;

          when READ_WAIT_ST => -- terminate or go for next pixel;
            if (rw_cntr < NUM_PIXELS) then 
              -- rw_cntr keeps track of how many times we read from sdram; we could
              -- use addr_buf2_r instead; but keep it this way in case later we'll
              -- need to increment addr_buf2_r by 2 or 4;
              rw_cntr <= rw_cntr + 1; 
					-- increment address for sdram controller;
		          addr_i_r <= addr_i_r + 14;
              addr_buf2_r <= addr_buf2_r + 1; -- increment address for buf2;  
              state <= READ_ST; 
            else
				  
              state <= DONE_ST;
            end if;
                 
                 
          -- when arrived to this state, a whole frame was written or read and we should
          -- stay here and not repeat the process of writing or reading a frame
          -- unless the whole thing is reset, which places us again in IDLE_ST state;
          when DONE_ST =>
					led_done_r <= '1';
					if (sw13_all_activate = '1' and sw6_auto_enable = '1' and take_snapshot1_auto = '1') then
						take_snapshot1_auto <= '0';
						timer_enable_frame2 <= '1';
						state <= idle_st;
					elsif (sw13_all_activate = '1' and sw6_auto_enable = '1' and take_snapshot2_auto = '1') then
						take_snapshot2_auto <= '0';
						state <= idle_st;
					else
						 state <= DONE_ST;
					end if;
				-- notify user it's success; will be used for self reset too at top_level;                
            
          -- part 2: states below related to read backs;
          -- state START_READ_ST is visited once only for each frame;
          when INIT_sub_st => 
				frame_number <= 1;
				rw_cntr  <= (others=>'0');
				addr_i_r <= (others => '0');
				addr_buf2_r <= (others=>'0');
				dout_buf2_r <= "110000000011";--yellow
				addr_buf1_r <= (others=>'0');
				Avg_red <= (others => '0');
				Avg_green <= (others => '0');
				Avg_blue <= (others => '0');
				Sum_red <= (others => '0');
				Sum_green <= (others => '0');
				Sum_blue <= (others => '0');
				state <= start_Read_Sub_st ;
			
			when Start_Read_Sub_st =>
				we_i_r <= '0';
				We_buf2_r <= '1';
				state <= Read_pixel_st;
				
          -- each pixel data reading process goes tru the sequence of 3 states:
          -- READ_ST --> WAIT_READ_ACK_ST --> READ_WAIT_ST
          -- which means three clock periods of 25 MHz; note that sdram
          -- controller operates with clk_100 that is much faster;    
			 
			 when Read_pixel_st => -- tell sdram_controller we want to read from addr_i_r
            stb_i_r <= '1';
            cyc_i_r <= '1';
            we_i_r <= '0';
            state <= wait_Read_Ack_pixel_st;
          
          when wait_Read_Ack_pixel_st => -- wait for controller which should assert ack_o 
            -- here we "stall" in this state until ack_o is asserted;
            if (ack_o = '1') then
              stb_i_r <= '0';
              cyc_i_r <= '0'; 
              dout_buf2_r <= dat_o_r(11 downto 0); -- what comes from sdram_controller is sent to buffer 2;
              state <= Read_Wait_pixel_st;
            end if;

          when Read_Wait_pixel_st => -- terminate or go for next pixel;
				addr_i_r <=  addr_i_r+2;
				addr_buf2_r <=  addr_buf2_r+1;
				if (frame_number<2) then
					frame_number <= frame_number+1;
					pixel_temp1 <=  Dout_buf2_r;
					
					state <=  Read_pixel_st;
				else
					pixel_temp2 <=  Dout_buf2_r;
					state <=  sub_and_avg_st;
				end if;
				
			when sub_and_avg_st =>
				if(pixel_temp2(11 downto 8)>pixel_temp1(11 downto 8))then
					sub_result(11 downto 8) <= (pixel_temp2(11 downto 8) - pixel_temp1(11 downto 8));
				else
					sub_result(11 downto 8) <= (pixel_temp1(11 downto 8) - pixel_temp2(11 downto 8));
				end if;
				
				Sum_red <= Sum_red + (sub_result(11 downto 8) & "0000"); 

			
				if(pixel_temp2(7 downto 4)>pixel_temp1(7 downto 4))then
					sub_result(7 downto 4) <= (pixel_temp2(7 downto 4) - pixel_temp1(7 downto 4));
				else
					sub_result(7 downto 4) <= (pixel_temp1(7 downto 4) - pixel_temp2(7 downto 4));
				end if;
				
				Sum_green <=  Sum_green + (sub_result(7 downto 4) & "0000"); 

				
				if(pixel_temp2(3 downto 0)>pixel_temp1(3 downto 0))then
					sub_result(3 downto 0) <= (pixel_temp2(3 downto 0) - pixel_temp1(3 downto 0));
				else
					sub_result(3 downto 0) <= (pixel_temp1(3 downto 0) - pixel_temp2(3 downto 0));
				end if;
				
				Sum_blue <=  Sum_blue + (sub_result(3 downto 0) & "0000"); 

				
				we_buf2_r <= '0';
				state <=  Start_Write_Sub_st;
          
          -- each pixel data writing into sdram goes tru the sequence of 3 states:
          -- WRITE_ST --> WAIT_WRITE_ACK_ST --> WRITE_WAIT_ST
          when Start_Write_Sub_st =>
				we_i_r <= '1';
				state <= write_Sub_st;

			 when write_Sub_st =>
            stb_i_r <= '1';
            cyc_i_r <= '1';
            we_i_r <= '1';
            -- now, this is a bit tricky: pick up data coming from buffer 1 here;
            -- data should be stable already, because I changed the address in the previous
            -- cycle inside state WRITE_WAIT_ST, but on falling edge of clk_i, from within
            -- a different process;
            --dat_i_r <= ("00000000000000000000" & "000000001111"); -- blue debug; 
				---- dat_i_r <= ("00000000000000000000" & din_buf1_r); -- pass this to be written to sdram;
            dat_i_r <= ("00000000000000000000" & sub_result);
				-- increment address for buf1; very important to do this here instead of
            -- in WRITE_WAIT_ST state; otherwise, we get a pixel lag that will shift the image
            -- to the right;
            addr_buf1_r <= addr_buf1_r + 1;
				state <= Wait_Write_Ack_Sub_st;

          when Wait_Write_Ack_Sub_st =>
            if (ack_o = '1') then
              stb_i_r <= '0';
              cyc_i_r <= '0';
				  state <= write_wait_Sub_and_avg_st;
            end if;
            
          when write_wait_Sub_and_avg_st =>
            if (rw_cntr < NUM_PIXELS) then
               rw_cntr <= rw_cntr + 1; -- keep track of how many times we write into sdram;
				   addr_i_r <= addr_i_r + 10; 
					frame_number <= 1;
               -- increment address for sdram controller;
					state <= Start_Read_Sub_st;
					
				else
					Avg_red <= std_logic_vector(to_unsigned( to_integer(unsigned(Sum_red))/to_integer(unsigned(num_pixels_avg)), 25));
					Avg_green <= std_logic_vector(to_unsigned(to_integer(unsigned(Sum_green))/to_integer(unsigned(num_pixels_avg)), 25));
					Avg_blue <= std_logic_vector(to_unsigned(to_integer(unsigned(Sum_blue))/to_integer(unsigned(num_pixels_avg)), 25));
              state <= done_Sub_and_avg_st;
            end if; 
                 
                 
          -- when arrived to this state, a whole frame was written or read and we should
          -- stay here and not repeat the process of writing or reading a frame
          -- unless the whole thing is reset, which places us again in IDLE_ST state;
          when done_Sub_and_avg_st =>
				led_sub_done_r <=  '1';
				led_done_r <=  '1';
				frameOneWrite <=  '0';
				frameTwoWrite <=  '0';
				if enable_all_sub_next = '1' then
					enable_all_sub_next <= '0';
					enable_all_sub <= '0';
					enable_all_thresh <= '1';
					enable_all_thresh_next <= '1';
					enable_all_median <= '0';
					enable_all_display <= '0';
					enable_sw13 <= '0';
				end if;
				flag_sub_done <= '1';
				state <= IDLE_ST;
				
			when start_Read_threshold_st =>
				We_i_r <= '0'; 
				We_buf2_r <= '1';
				State <=  Read_threshold_st;
				
			when Read_threshold_st =>
				State <= Wait_read_ack_threshold_st;
				Stb_i_r <=  '1';
				cyc_i_r <= '1';
				We_i_r <= '0';
				
			when Wait_read_ack_threshold_st =>
				if (ack_o='1') then
					State <=  Read_Wait_threshold_st;
					dout_buf2_r <= dat_o_r (11 downto 0);

				end if;

			when Read_Wait_threshold_st =>
				Red_pixel <=  Dout_buf2_r (11 downto 8) & "0000";
				green_pixel <=  Dout_buf2_r (7 downto 4) & "0000";
				blue_pixel <=  Dout_buf2_r (3 downto 0) & "0000";			
				State <=  threshold_st;
				Addr_i_r <=  addr_i_r + 2;
				Addr_buf2_r <=  Addr_buf2_r+1;

			when threshold_st=>	
					If (red_pixel<Avg_red+20 or to_integer(unsigned(rw_cntr)) mod 320 = 319) then 
						 thresh_result(11 downto 8)  <= "0000";
					Else 
						 thresh_result(11 downto 8)  <= "1111";
					End if;

					If (green_pixel<Avg_green+20  or to_integer(unsigned(rw_cntr)) mod 320 = 319) then 
						 thresh_result(7 downto 4)  <= "0000";
					Else 
						 thresh_result(7 downto 4)  <= "1111";
					End if;

					If (blue_pixel<Avg_blue+20  or to_integer(unsigned(rw_cntr)) mod 320 = 319) then 
						 thresh_result(3 downto 0)  <= "0000";
					Else 
						 thresh_result(3 downto 0)  <= "1111";
					End if;
				state <= Start_Write_Threshold_st;

				
			when Start_Write_Threshold_st =>
				State <= write_threshold_st;
				We_i_r <= '1';
				
			when write_threshold_st =>
				State <=  Wait_Write_threshold_Ack_st;
				Stb_i_r <=  '1';
				cyc_i_r <= '1';
				We_i_r <= '1';
				Dat_i_r <= "00000000000000000000" & thresh_result ;
				Addr_buf1_r <= addr_buf1_r+1;

			when Wait_Write_threshold_Ack_st =>
				if(ack_o='1')then
					  State <=  write_wait_threshold_st;
					  Stb_i_r <= '0';
					  Cyc_i_r <= '0';
					 end if;
				
			when write_wait_threshold_st =>
				if(rw_cntr<num_pixels)then
						State <=  start_Read_threshold_st;
						Rw_cntr <= rw_cntr+1;
						addr_i_r <=  addr_i_r+12;
				else
						State <= done_threshold_st;
				end if;
				
			when done_threshold_st =>
				led_sub_done_r <=  '0';
				led_done_r <=  '0';
				frameOneWrite <=  '0';
				frameTwoWrite <=  '0';
				Flag_sub_done <= '0';
				flag_thresh_done <= '1';
				if enable_all_thresh_next = '1' then
					enable_all_sub <= '0';
					enable_all_thresh <= '0';
					enable_all_thresh_next <= '0';
					enable_all_median <= '1';
					enable_all_median_next <= '1';
					enable_all_display <= '0';
					enable_sw13 <= '0';
				end if;				
				state <= IDLE_ST ;
				
			when check_pixel_location_median_st =>
				ones_counter := 0;
				state <= Start_Read_median_st;
				
			when Start_Read_median_st =>
				State <= Read_median_st;
				We_i_r <= '0';
				median49counter := 1;
				We_buf2_r <= '1';
				addr_i_r <= addr_i_r - 13482;

			when Read_median_st =>
				State <= Wait_Read_Ack_median_st;
				Stb_i_r <=  '1';
				cyc_i_r <= '1';
				We_i_r <= '0';
				
			when Wait_Read_Ack_median_st =>
				if (ack_o='1') then
					State <= Read_Wait_median_st;
					if dat_o_r (0) = '1' then
						ones_counter := ones_counter + 1;
					end if;
				end if;
				
			when Read_Wait_median_st =>
				median49counter := median49counter + 1;
				if median49counter = 50 then
					addr_i_r  <= addr_i_r - 13480;
					state <= find_median_st;
				Elsif median49counter mod 7 = 1 then
					addr_i_r <= addr_i_r + 4396;
					state <= Read_median_st;
				Else
					addr_i_r <= addr_i_r + 14;
					state <= Read_median_st;
				End if;
				

			when find_median_st =>

				If to_integer(unsigned(rw_cntr)) < 963 or to_integer(unsigned(rw_cntr)) > 75836 or to_integer(unsigned(rw_cntr)) mod 320 > 316 or to_integer(unsigned(rw_cntr)) mod 320 < 3 then
					median_value <= '0';
				else
					if ones_counter > 24 then
						median_value <= '1';
					else
						median_value <= '0';
					end if;
				end if;
				state <= Start_Write_median_st;
			
			when Start_Write_median_st =>
				State <= write_median_st;
				We_i_r <= '1';
				
			when write_median_st =>
				State <=  address_change_st;
				Stb_i_r <=  '1';
				cyc_i_r <= '1';
				We_i_r <= '1';
				if median_value ='1' then
					Dat_i_r <= "00000000000000000000" & "111111111111";
				else
					Dat_i_r <= "00000000000000000000" & "000000000000";
				end if;
				
			when address_change_st =>
				addr_i_r <= addr_i_r + 2;
				State <= write_location_median_st;
				We_i_r <= '1';
				
			when write_location_median_st =>
				State <=  Wait_Write_Ack_median_st;
				Stb_i_r <=  '1';
				cyc_i_r <= '1';
				We_i_r <= '1';
				if median_value ='1' then
					Dat_i_r <= "00000000000000000000" & "111111111111";
					if to_integer(unsigned(rw_cntr)) mod 320 > to_integer(unsigned(right_pixel)) mod 320 then
						right_pixel <= rw_cntr;
					end if;
					if to_integer(unsigned(rw_cntr)) mod 320 < to_integer(unsigned(left_pixel)) mod 320 and to_integer(unsigned(rw_cntr)) mod 320 > 4 and to_integer(unsigned(rw_cntr)) mod 320 /= 63 then
						left_pixel <= rw_cntr;
					end if;
					if to_integer(unsigned(rw_cntr)) / 320 > to_integer(unsigned(down_pixel)) / 320 then
						down_pixel <= rw_cntr;
					end if;
					if to_integer(unsigned(rw_cntr)) / 320 < to_integer(unsigned(up_pixel)) / 320 then
						up_pixel <= rw_cntr;
					end if;
				else
					Dat_i_r <= "00000000000000000000" & "000000000000";
				end if;
				Addr_buf1_r <= addr_buf1_r+1;
				
			when Wait_Write_Ack_median_st =>
				If(ack_o='1')then
				  State <=  write_wait_median_st;
				  Stb_i_r <= '0';
				  Cyc_i_r <= '0';
				End if;	
				
			when write_wait_median_st =>
				If(rw_cntr<NUM_PIXELS) then
					rw_cntr <= rw_cntr + 1;
					addr_i_r <= addr_i_r + 10;
					State <= check_pixel_location_median_st;
				Else
					State <= done_median_st;
				End if;

			
			when done_median_st =>
				Flag_median_done <= '1';
				if enable_all_median_next = '1' then
					enable_all_sub <= '0';
					enable_all_thresh <= '0';
					enable_all_median <= '0';
					enable_all_median_next <= '0';
					enable_all_display <= '1';
					enable_sw13 <= '1';
				end if;
				state <= calculate_location_st ;
				
			when calculate_location_st =>
				x_left := to_integer(unsigned(left_pixel) mod 320);
				x_right := to_integer(unsigned(right_pixel) mod 320);
				x := (x_right+x_left)/2;
				y := ((to_integer(unsigned(down_pixel))/320) + (to_integer(unsigned(up_pixel))/320))/2;
				location_pixel := y*320 + x;
				address_pixel <= 10 + 14*location_pixel;
				state <= Start_Write_location_st;

			when Start_Write_location_st =>
				State <= write_location_st;
				We_i_r <= '1';
				mark_counter := 1;
				
				addr_i_r <= std_logic_vector(to_unsigned(address_pixel, 25)) - 44800;
				
			when write_location_st =>
				State <=  Wait_Write_Ack_location_st;
				Stb_i_r <=  '1';
				cyc_i_r <= '1';
				We_i_r <= '1';
				Dat_i_r <= "00000000000000000000" & "111100000000";
				Addr_buf1_r <= addr_buf1_r+1;

			when Wait_Write_Ack_location_st =>
				If(ack_o='1')then
				  State <=  write_wait_location_st;
				  Stb_i_r <= '0';
				  Cyc_i_r <= '0';
				End if;	
				
			when write_wait_location_st =>
			
			mark_counter := mark_counter + 1;
			
			
				If (mark_counter < 22) then
					addr_i_r <= addr_i_r + 4480;
					state <= write_location_st;
				elsif (mark_counter = 22)	then
					addr_i_r <= addr_i_r - 44940;
					state <= write_location_st;
				elsif (mark_counter < 44) then
					addr_i_r <= addr_i_r + 14;
					state <= write_location_st;
				elsif (mark_counter = 44)	then
					state <= done_location_st;
				end if;
			
			when done_location_st =>
				led_done_r <=  '1';
				Flag_location_done <= '1';
				cmd_engine_x <= x - 159;
				cmd_engine_y <= y - 119;
				trigger <= '1';
				left_pixel <= "010010101111111111";
				right_pixel <= (others => '0'); 
				up_pixel <= "010010100110000000"; 
				down_pixel <= (others => '0');
				state <= IDLE_ST ;
     			if (sw6_auto_enable = '1' and sw13_all_activate = '1') then
					timer_enable_frame1 <= '1';
				end if;
		
			-- IDLE state
          when others => 
            state <= IDLE_ST;
            led_done_r <= '0';
				led_sub_done_r <=  '0';
				
				 if (sw6_auto_enable = '1' and btn_take_snapshot = '0' and sw13_all_activate='1') then
					take_snapshot1_auto <= '1';
					  
				 elsif (sw6_auto_enable = '1' and timer_enable_frame1 = '1' and sw13_all_activate='1') then
					if r_CNT = 100000000-1 then  -- -1, since counter starts at 0
						take_snapshot1_auto <= '1';
						timer_enable_frame1 <= '0';
						r_CNT <= 0;
					else
						r_CNT <= r_CNT + 1;
					end if;
					  
					  
				 elsif (sw6_auto_enable = '1' and timer_enable_frame2 = '1' and sw13_all_activate='1') then
					if r_CNT = 75000000-1 then  -- -1, since counter starts at 0
						take_snapshot2_auto <= '1';
						timer_enable_frame2 <= '0';
						r_CNT <= 0;
					else
						r_CNT <= r_CNT + 1;
					end if;

				elsif (take_snapshot1 = '1' or take_snapshot1_auto = '1') then
				  state <= START_WRITE_ST;
				  led_done_r <= '0';
				  we_buf2_r <= '0';
				  led_sub_done_r <= '0';
				  addr_buf1_r <= (others => '0');
				  
				elsif (display_snapshot1 = '1') then
				  state <= START_READ_ST;
				  led_done_r <= '0';
				  we_buf2_r <= '1';
				  led_sub_done_r <= '0';
				  addr_buf2_r  <= (others => '0');
				  
				elsif (take_snapshot2 = '1' or take_snapshot2_auto = '1') then
				  state <= START_WRITE_ST;
				  led_done_r <= '0';
				  led_sub_done_r <= '0';
				  we_buf2_r <= '0';
					addr_buf1_r  <= (others => '0');
					
				elsif (display_snapshot2 = '1') then
				  state <= START_READ_ST;
				  led_done_r <= '0';
				  led_sub_done_r <= '0';
				  we_buf2_r <= '1';
				  addr_buf2_r <= (others => '0');  
				  
				elsif (display_sub = '1') then
				  state <= START_READ_ST;
				  led_done_r <= '0';
				  led_sub_done_r <= '0';
				  we_buf2_r <= '1';
				  addr_buf2_r <= (others => '0');
				  
				elsif ((sw8_sub_activate= '1' and frameOneWrite='1' and frameTwoWrite='1') or enable_all_sub='1') then      
				  state <= INIT_sub_st;	
				  enable_all_sub <= '0';
				  led_done_r <= '0'; 
				  led_sub_done_r <= '0';
				  
				elsif ((sw9_thresh_activate='1' and flag_sub_done='1') or enable_all_thresh = '1') then
					state <= start_Read_threshold_st;
					enable_all_thresh <= '0';
					addr_i_r <= "0000000000000000000000100";
					rw_cntr <= (others=>'0');
					addr_buf2_r <= (others=>'0');
					addr_buf1_r <= (others=>'0');
					led_done_r <= '0'; 
					led_sub_done_r <= '0';
					
				elsif (display_threshold = '1') then
				  state <= START_READ_ST;
				  led_done_r <= '0';
				  led_sub_done_r <= '0';
				  we_buf2_r <= '1';
				  addr_buf2_r <= (others => '0');
				  
				elsif ((sw11_median_activate = '1' and flag_thresh_done = '1') or enable_all_median = '1') then
					flag_thresh_done <= '0';
					enable_all_median <= '0';
					led_done_r <= '0'; 
					led_sub_done_r <= '0';
					addr_i_r  <= "0000000000000000000000110";
					rw_cntr <= (others=>'0');
					addr_buf2_r <= (others=>'0');
					Addr_buf1_r <= (others=>'0');
					Dout_buf2_r <= "110000000011";
					State <=  check_pixel_location_median_st;
					
				elsif ((display_median = '1') or (display_all = '1' and enable_all_display ='1')) then
				  state <= START_READ_ST;
				  enable_all_display <= '0';
				  led_done_r <= '0';
				  led_sub_done_r <= '0';
				  we_buf2_r <= '1';
				  addr_buf2_r <= (others => '0');
				  
				elsif (display_location = '1') then
				  state <= START_READ_ST;
				  led_done_r <= '0';
				  led_sub_done_r <= '0';
				  we_buf2_r <= '1';
				  addr_buf2_r <= (others => '0');
				  
				elsif (sw13_all_activate = '1' and frameOneWrite='1' and frameTwoWrite='1' and enable_sw13 = '1') then
					enable_all_sub <= '1';
					enable_all_sub_next <= '1';
					enable_sw13 <= '0';
				end if;
			
--				if (cmd_engine_x >= 0) then
					temp1 <= std_logic_vector(to_unsigned(x, 18));
--				else		
--					temp1 <= std_logic_vector(to_unsigned(90000-cmd_engine_x, 18));
--				end if;
--				
--				if (cmd_engine_y >= 0) then
					temp2 <= std_logic_vector(to_unsigned(y, 18));
--				else			
--					temp2 <= std_logic_vector(to_unsigned(90000-cmd_engine_y, 18));
--				end if;
				
				if ack_trigger = '1' then
					trigger <= '0';
				end if;
				
				
           
        end case;
      end if;
    end if;
end process;	
  


end my_behavioral;



-- ok, let's brush up our French here; :-)

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity VGA is
  Port ( 
    CLK25 : in  STD_LOGIC;         -- Horloge d'entre de 25 MHz              
    clkout : out  STD_LOGIC;       -- Horloge de sortie vers le ADV7123 et l'ecran TFT
    Hsync,Vsync : out  STD_LOGIC;  -- les deux signaux de synchronisation pour l'ecran VGA
    Nblank : out  STD_LOGIC;       -- signal de commande du convertisseur N/A ADV7123
    activeArea : out  STD_LOGIC;
    Nsync : out  STD_LOGIC         -- signaux de synchronisation et commande de l'ecran TFT
  );        
end VGA;


architecture Behavioral of VGA is


signal Hcnt:STD_LOGIC_VECTOR(9 downto 0) := "0000000000"; -- pour le comptage des colonnes
signal Vcnt:STD_LOGIC_VECTOR(9 downto 0) := "1000001000"; -- pour le comptage des lignes
signal video:STD_LOGIC;
constant HM: integer := 799;  --la taille maximale considere 800 (horizontal)
constant HD: integer := 640;  --la taille de l'ecran (horizontal)
constant HF: integer := 16;   --front porch
constant HB: integer := 48;   --back porch
constant HR: integer := 96;   --sync time
constant VM: integer := 524;  --la taille maximale considere 525 (vertical)  
constant VD: integer := 480;  --la taille de l'ecran (vertical)
constant VF: integer := 10;   --front porch
constant VB: integer := 33;   --back porch
constant VR: integer := 2;    --retrace


begin


-- initialisation d'un compteur de 0 a 799 (800 pixel par ligne):
-- a chaque front d'horloge en incremente le compteur de colonnes
-- c-a-d du 0 a 799.
  process(CLK25)
  begin
    if (CLK25'event and CLK25='1') then
      if (Hcnt = HM) then -- 799
        Hcnt <= "0000000000";
        if (Vcnt= VM) then -- 524
          Vcnt <= "0000000000";
          activeArea <= '1';
        else
          if vCnt < 240-1 then
            activeArea <= '1';
          end if;
          Vcnt <= Vcnt+1;
        end if;
      else      
        if hcnt = 320-1 then
          activeArea <= '0';
        end if;
        Hcnt <= Hcnt + 1;
      end if;
    end if;
  end process;
  
  
-- generation du signal de synchronisation horizontale Hsync:
  process(CLK25)
  begin
    if (CLK25'event and CLK25='1') then
      if (Hcnt >= (HD+HF) and Hcnt <= (HD+HF+HR-1)) then -- Hcnt >= 656 and Hcnt <= 751
        Hsync <= '0';
      else
        Hsync <= '1';
      end if;
    end if;
  end process;


-- generation du signal de synchronisation verticale Vsync:
  process(CLK25)
  begin
    if (CLK25'event and CLK25='1') then
      if (Vcnt >= (VD+VF) and Vcnt <= (VD+VF+VR-1)) then  ---Vcnt >= 490 and vcnt <=  491
        Vsync <= '0';
      else
        Vsync <= '1';
      end if;
    end if;
  end process;


-- Nblank et Nsync pour commander le covertisseur ADV7123:
Nsync <= '1';
video <= '1' when (Hcnt < HD) and (Vcnt < VD) -- c'est pour utiliser la resolution complete 640 x 480  
        else '0';
Nblank <= video;
clkout <= CLK25; 
end Behavioral;
