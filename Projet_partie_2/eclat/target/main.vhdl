-- code generated from the following source code:
--   stdlib.ecl
--   code.ecl
--   main.ecl
--
-- with the following command:
--
--    C:\Users\DELL\Desktop\CA\Projet2-CA\Projet_CAM\eclat\eclat-compiler\eclat.exe -arg=true code.ecl main.ecl

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal argument : in value(0 to 0);
       signal result : out value(0 to 0));
       
end entity;
architecture rtl of main is

  type t_state is (IDLE61, PAUSE_SET77, Q_WAIT78);
  signal \state%now\, \state%next\: t_state;
  type t_state_var80 is (IDLE64, \$372\, PAUSE_GET68, PAUSE_SET72, Q_WAIT69, Q_WAIT73);
  signal \state_var80%now\, \state_var80%next\: t_state_var80;
  type array_value_32 is array (natural range <>) of value(0 to 31);
  type array_value_36 is array (natural range <>) of value(0 to 35);
  signal \$315_code\ : array_value_36(0 to 0);
  signal \$$315_code_value\ : value(0 to 35) := (others => '0');
  signal \$$315_code_ptr\ : natural range 0 to 0 := 0;
  signal \$$315_code_ptr_write\ : natural range 0 to 0 := 0;
  signal \$$315_code_write\ : value(0 to 35) := (others => '0');
  signal \$$315_code_write_request\ : std_logic := '0';
  signal \$317_code\ : array_value_36(0 to 1023);
  signal \$$317_code_value\ : value(0 to 35) := (others => '0');
  signal \$$317_code_ptr\ : natural range 0 to 1023 := 0;
  signal \$$317_code_ptr_write\ : natural range 0 to 1023 := 0;
  signal \$$317_code_write\ : value(0 to 35) := (others => '0');
  signal \$$317_code_write_request\ : std_logic := '0';
  signal \$318_stack\ : array_value_32(0 to 255);
  signal \$$318_stack_value\ : value(0 to 31) := (others => '0');
  signal \$$318_stack_ptr\ : natural range 0 to 255 := 0;
  signal \$$318_stack_ptr_write\ : natural range 0 to 255 := 0;
  signal \$$318_stack_write\ : value(0 to 31) := (others => '0');
  signal \$$318_stack_write_request\ : std_logic := '0';
  signal \$375%next\, \$375%now\ : value(0 to 35) := (others => '0');
  signal \$367%next\, \$367%now\ : value(0 to 32) := (others => '0');
  signal \$372_arg%next\, \$372_arg%now\ : value(0 to 33) := (others => '0');
  signal \$v67%next\, \$v67%now\ : value(0 to 3) := (others => '0');
  signal \$372_id%next\, \$372_id%now\ : value(0 to 11) := (others => '0');
  signal \$$318_stack_lock%next\, \$$318_stack_lock%now\, 
         \$$317_code_lock%next\, \$$317_code_lock%now\, 
         \$$315_code_lock%next\, \$$315_code_lock%now\, \rdy63%next\, 
         \rdy63%now\, \$v70%next\, \$v70%now\, \result59%next\, 
         \result59%now\, \$369%next\, \$369%now\, \$377%next\, \$377%now\, 
         \rdy60%next\, \rdy60%now\, \$v56%next\, \$v56%now\, \$v74%next\, 
         \$v74%now\, \$v53%next\, \$v53%now\, \$v54%next\, \$v54%now\, 
         \$v79%next\, \$v79%now\, \$368%next\, \$368%now\, \$361_w%next\, 
         \$361_w%now\, \$v55%next\, \$v55%now\ : value(0 to 0) := (others => '0');
  signal \$364_r%next\, \$364_r%now\, \$379%next\, \$379%now\, 
         \$363_pc%next\, \$363_pc%now\, \$362_sp%next\, \$362_sp%now\, 
         \$376_v%next\, \$376_v%now\, \result62%next\, \result62%now\, 
         \$v71%next\, \$v71%now\, \$381%next\, \$381%now\, \$380%next\, 
         \$380%now\, \$366_cy%next\, \$366_cy%now\, \$378%next\, \$378%now\, 
         \$v76%next\, \$v76%now\, \$v65%next\, \$v65%now\ : value(0 to 31) := (others => '0');
  
  begin
    process (clk)
            begin
            if rising_edge(clk) then
                 if \$$315_code_write_request\ = '1' then
                    \$315_code\(\$$315_code_ptr_write\) <= \$$315_code_write\;
                 end if;
                 \$$315_code_value\ <= \$315_code\(\$$315_code_ptr\);
            end if;
        end process;
    
    process (clk)
            begin
            if rising_edge(clk) then
                 if \$$317_code_write_request\ = '1' then
                    \$317_code\(\$$317_code_ptr_write\) <= \$$317_code_write\;
                 end if;
                 \$$317_code_value\ <= \$317_code\(\$$317_code_ptr\);
            end if;
        end process;
    
    process (clk)
            begin
            if rising_edge(clk) then
                 if \$$318_stack_write_request\ = '1' then
                    \$318_stack\(\$$318_stack_ptr_write\) <= \$$318_stack_write\;
                 end if;
                 \$$318_stack_value\ <= \$318_stack\(\$$318_stack_ptr\);
            end if;
        end process;
    
    process (reset,clk)
      begin
      if reset = '1' then
        \$367%now\ <= (others => '0');
        \$v65%now\ <= (others => '0');
        \$v55%now\ <= (others => '0');
        \$372_arg%now\ <= (others => '0');
        \$361_w%now\ <= (others => '0');
        \$368%now\ <= (others => '0');
        \$v76%now\ <= (others => '0');
        \$v79%now\ <= (others => '0');
        \$v54%now\ <= (others => '0');
        \$378%now\ <= (others => '0');
        \$v53%now\ <= (others => '0');
        \$372_id%now\ <= (others => '0');
        \$v74%now\ <= (others => '0');
        \$v56%now\ <= (others => '0');
        \$375%now\ <= (others => '0');
        \rdy60%now\ <= (others => '0');
        \$377%now\ <= (others => '0');
        \$366_cy%now\ <= (others => '0');
        \$369%now\ <= (others => '0');
        \$380%now\ <= (others => '0');
        \result59%now\ <= (others => '0');
        \$v67%now\ <= (others => '0');
        \$381%now\ <= (others => '0');
        \$v71%now\ <= (others => '0');
        \result62%now\ <= (others => '0');
        \$376_v%now\ <= (others => '0');
        \$362_sp%now\ <= (others => '0');
        \$363_pc%now\ <= (others => '0');
        \$v70%now\ <= (others => '0');
        \$379%now\ <= (others => '0');
        \rdy63%now\ <= (others => '0');
        \$364_r%now\ <= (others => '0');
        \$$315_code_lock%now\ <= (others => '0');
        \$$317_code_lock%now\ <= (others => '0');
        \$$318_stack_lock%now\ <= (others => '0');
        \state%now\ <= idle61;
        \state_var80%now\ <= idle64;
      elsif (rising_edge(clk)) then
        \$367%now\ <= \$367%next\;
        \$v65%now\ <= \$v65%next\;
        \$v55%now\ <= \$v55%next\;
        \$372_arg%now\ <= \$372_arg%next\;
        \$361_w%now\ <= \$361_w%next\;
        \$368%now\ <= \$368%next\;
        \$v76%now\ <= \$v76%next\;
        \$v79%now\ <= \$v79%next\;
        \$v54%now\ <= \$v54%next\;
        \$378%now\ <= \$378%next\;
        \$v53%now\ <= \$v53%next\;
        \$372_id%now\ <= \$372_id%next\;
        \$v74%now\ <= \$v74%next\;
        \$v56%now\ <= \$v56%next\;
        \$375%now\ <= \$375%next\;
        \rdy60%now\ <= \rdy60%next\;
        \$377%now\ <= \$377%next\;
        \$366_cy%now\ <= \$366_cy%next\;
        \$369%now\ <= \$369%next\;
        \$380%now\ <= \$380%next\;
        \result59%now\ <= \result59%next\;
        \$v67%now\ <= \$v67%next\;
        \$381%now\ <= \$381%next\;
        \$v71%now\ <= \$v71%next\;
        \result62%now\ <= \result62%next\;
        \$376_v%now\ <= \$376_v%next\;
        \$362_sp%now\ <= \$362_sp%next\;
        \$363_pc%now\ <= \$363_pc%next\;
        \$v70%now\ <= \$v70%next\;
        \$379%now\ <= \$379%next\;
        \rdy63%now\ <= \rdy63%next\;
        \$364_r%now\ <= \$364_r%next\;
        \$$315_code_lock%now\ <= \$$315_code_lock%next\;
        \$$317_code_lock%now\ <= \$$317_code_lock%next\;
        \$$318_stack_lock%now\ <= \$$318_stack_lock%next\;
        \state_var80%now\ <= \state_var80%next\;
        \state%now\ <= \state%next\;
      end if;
    end process;
      
      process(argument,\state%now\, clk,\state_var80%now\, \$$315_code_value\, \$$317_code_value\, \$$318_stack_value\, \$367%now\, \$v65%now\, \$v55%now\, \$372_arg%now\, \$361_w%now\, \$368%now\, \$v76%now\, \$v79%now\, \$v54%now\, \$378%now\, \$v53%now\, \$372_id%now\, \$v74%now\, \$v56%now\, \$375%now\, \rdy60%now\, \$377%now\, \$366_cy%now\, \$369%now\, \$380%now\, \result59%now\, \$v67%now\, \$381%now\, \$v71%now\, \result62%now\, \$376_v%now\, \$362_sp%now\, \$363_pc%now\, \$v70%now\, \$379%now\, \rdy63%now\, \$364_r%now\, \$$315_code_lock%now\, \$$317_code_lock%now\, \$$318_stack_lock%now\)
        variable \$375\ : value(0 to 35) := (others => '0');
        variable \$367\ : value(0 to 32) := (others => '0');
        variable \$372_arg\ : value(0 to 33) := (others => '0');
        variable \$v67\ : value(0 to 3) := (others => '0');
        variable \$372_id\ : value(0 to 11) := (others => '0');
        variable rdy63, \$v70\, result59, \$369\, \$377\, rdy60, \$v56\, 
                 \$v74\, \$v53\, \$v54\, \$v79\, \$368\, \$361_w\, \$v55\ : value(0 to 0) := (others => '0');
        variable \$364_r\, \$379\, \$363_pc\, \$362_sp\, \$376_v\, result62, 
                 \$v71\, \$381\, \$380\, \$366_cy\, \$378\, \$v76\, \$v65\ : value(0 to 31) := (others => '0');
        variable state : t_state;
        variable state_var80 : t_state_var80;
        variable \$$315_code_lock\ : value(0 to 0);
        variable \$$317_code_lock\ : value(0 to 0);
        variable \$$318_stack_lock\ : value(0 to 0);
        
    begin
      \$367\ := \$367%now\;
      \$v65\ := \$v65%now\;
      \$v55\ := \$v55%now\;
      \$372_arg\ := \$372_arg%now\;
      \$361_w\ := \$361_w%now\;
      \$368\ := \$368%now\;
      \$v76\ := \$v76%now\;
      \$v79\ := \$v79%now\;
      \$v54\ := \$v54%now\;
      \$378\ := \$378%now\;
      \$v53\ := \$v53%now\;
      \$372_id\ := \$372_id%now\;
      \$v74\ := \$v74%now\;
      \$v56\ := \$v56%now\;
      \$375\ := \$375%now\;
      rdy60 := \rdy60%now\;
      \$377\ := \$377%now\;
      \$366_cy\ := \$366_cy%now\;
      \$369\ := \$369%now\;
      \$380\ := \$380%now\;
      result59 := \result59%now\;
      \$v67\ := \$v67%now\;
      \$381\ := \$381%now\;
      \$v71\ := \$v71%now\;
      result62 := \result62%now\;
      \$376_v\ := \$376_v%now\;
      \$362_sp\ := \$362_sp%now\;
      \$363_pc\ := \$363_pc%now\;
      \$v70\ := \$v70%now\;
      \$379\ := \$379%now\;
      rdy63 := \rdy63%now\;
      \$364_r\ := \$364_r%now\;
      \$$315_code_lock\ := \$$315_code_lock%now\;
      \$$317_code_lock\ := \$$317_code_lock%now\;
      \$$318_stack_lock\ := \$$318_stack_lock%now\;
      state := \state%now\;
      state_var80 := \state_var80%now\;
      case state is
      when PAUSE_SET77 =>
        \$$315_code_write_request\ <= '0';
        release(\$$315_code_lock\);
        \$361_w\ := eclat_unit;
        if \$v53\(0) = '1' then
          
        else
          \$v53\ := eclat_true;
          \$381\ := X"0000000" & X"0";
        end if;
        \$381\ := work.Int.add(\$381\, X"0000000" & X"1");
        \$362_sp\ := \$381\;
        if \$v54\(0) = '1' then
          
        else
          \$v54\ := eclat_true;
          \$380\ := X"0000000" & X"0";
        end if;
        \$380\ := work.Int.add(\$380\, X"0000000" & X"1");
        \$363_pc\ := \$380\;
        if \$v55\(0) = '1' then
          
        else
          \$v55\ := eclat_true;
          \$379\ := X"0000000" & X"0";
        end if;
        \$379\ := work.Int.add(\$379\, X"0000000" & X"1");
        \$364_r\ := \$379\;
        if \$v56\(0) = '1' then
          
        else
          \$v56\ := eclat_true;
          \$378\ := X"0000000" & X"0";
        end if;
        \$378\ := work.Int.add(\$378\, X"0000000" & X"1");
        \$366_cy\ := \$378\;
        case state_var80 is
        when \$372\ =>
          \$v70\ := \$$317_code_lock\;
          if \$v70\(0) = '1' then
            state_var80 := Q_WAIT69;
          else
            acquire(\$$317_code_lock\);
            \$$317_code_ptr\ <= to_integer(unsigned(\$372_arg\(2 to 33)));
            state_var80 := PAUSE_GET68;
          end if;
        when PAUSE_GET68 =>
          \$375\ := \$$317_code_value\;
          release(\$$317_code_lock\);
          \$v67\ := \$375\(0 to 3);
          \$v65\ := \$375\(4 to 35);
          case \$v67\ is
          when "0000" =>
            \$376_v\ := \$v65\(0 to 31);
            \$377\ := work.Int.print(clk,\$376_v\);
            \$372_arg\ := eclat_unit & eclat_unit & \$372_arg\(2 to 33);
            state_var80 := \$372\;
          when others =>
            \$372_arg\ := eclat_unit & eclat_unit & \$372_arg\(2 to 33);
            state_var80 := \$372\;
          end case;
        when PAUSE_SET72 =>
          \$$317_code_write_request\ <= '0';
          release(\$$317_code_lock\);
          \$368\ := eclat_unit;
          \$369\ := work.Print.print_string(clk,of_string("début\n"));
          \$372_id\ := "000000000001";
          \$372_arg\ := eclat_unit & eclat_unit & \$363_pc\;
          state_var80 := \$372\;
        when Q_WAIT69 =>
          \$v70\ := \$$317_code_lock\;
          if \$v70\(0) = '1' then
            state_var80 := Q_WAIT69;
          else
            acquire(\$$317_code_lock\);
            \$$317_code_ptr\ <= to_integer(unsigned(\$372_arg\(2 to 33)));
            state_var80 := PAUSE_GET68;
          end if;
        when Q_WAIT73 =>
          \$v74\ := \$$317_code_lock\;
          if \$v74\(0) = '1' then
            state_var80 := Q_WAIT73;
          else
            acquire(\$$317_code_lock\);
            \$v71\ := X"000000" & X"2a";
            \$$317_code_ptr_write\ <= 0;\$$317_code_write\ <= "0000" & \$v71\; \$$317_code_write_request\ <= '1';
            state_var80 := PAUSE_SET72;
          end if;
        when IDLE64 =>
          rdy63 := eclat_false;
          \$v74\ := \$$317_code_lock\;
          if \$v74\(0) = '1' then
            state_var80 := Q_WAIT73;
          else
            acquire(\$$317_code_lock\);
            \$v71\ := X"000000" & X"2a";
            \$$317_code_ptr_write\ <= 0;\$$317_code_write\ <= "0000" & \$v71\; \$$317_code_write_request\ <= '1';
            state_var80 := PAUSE_SET72;
          end if;
        end case;
        
        if rdy63(0) = '1' then
          
        else
          result62 := X"0000000" & X"0";
        end if;
        \$367\ := result62 & rdy63;
        result59 := work.Bool.lnot(""&\$367\(32));
        rdy60 := eclat_true;
        state := IDLE61;
      when Q_WAIT78 =>
        \$v79\ := \$$315_code_lock\;
        if \$v79\(0) = '1' then
          state := Q_WAIT78;
        else
          acquire(\$$315_code_lock\);
          \$v76\ := X"0000000" & X"5";
          \$$315_code_ptr_write\ <= 0;\$$315_code_write\ <= "0000" & \$v76\; \$$315_code_write_request\ <= '1';
          state := PAUSE_SET77;
        end if;
      when IDLE61 =>
        rdy60 := eclat_false;
        \$v79\ := \$$315_code_lock\;
        if \$v79\(0) = '1' then
          state := Q_WAIT78;
        else
          acquire(\$$315_code_lock\);
          \$v76\ := X"0000000" & X"5";
          \$$315_code_ptr_write\ <= 0;\$$315_code_write\ <= "0000" & \$v76\; \$$315_code_write_request\ <= '1';
          state := PAUSE_SET77;
        end if;
      end case;
      \state%next\ <= state;
      \state_var80%next\ <= state_var80;
      \$367%next\ <= \$367\;
      \$v65%next\ <= \$v65\;
      \$v55%next\ <= \$v55\;
      \$372_arg%next\ <= \$372_arg\;
      \$361_w%next\ <= \$361_w\;
      \$368%next\ <= \$368\;
      \$v76%next\ <= \$v76\;
      \$v79%next\ <= \$v79\;
      \$v54%next\ <= \$v54\;
      \$378%next\ <= \$378\;
      \$v53%next\ <= \$v53\;
      \$372_id%next\ <= \$372_id\;
      \$v74%next\ <= \$v74\;
      \$v56%next\ <= \$v56\;
      \$375%next\ <= \$375\;
      \rdy60%next\ <= rdy60;
      \$377%next\ <= \$377\;
      \$366_cy%next\ <= \$366_cy\;
      \$369%next\ <= \$369\;
      \$380%next\ <= \$380\;
      \result59%next\ <= result59;
      \$v67%next\ <= \$v67\;
      \$381%next\ <= \$381\;
      \$v71%next\ <= \$v71\;
      \result62%next\ <= result62;
      \$376_v%next\ <= \$376_v\;
      \$362_sp%next\ <= \$362_sp\;
      \$363_pc%next\ <= \$363_pc\;
      \$v70%next\ <= \$v70\;
      \$379%next\ <= \$379\;
      \rdy63%next\ <= rdy63;
      \$364_r%next\ <= \$364_r\;
      \$$315_code_lock%next\ <= \$$315_code_lock\;
      \$$317_code_lock%next\ <= \$$317_code_lock\;
      \$$318_stack_lock%next\ <= \$$318_stack_lock\;
      
      
      result <= result59;
      end process;
  end architecture;
