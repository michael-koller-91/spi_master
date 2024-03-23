library ieee;
use ieee.std_logic_1164.all;

entity spi_master is
  generic (
    G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE : boolean    := true;
    G_N_CLKS_SCS_TO_SCLK                      : positive   := 1;  -- natural?
    G_N_CLKS_SCLK_TO_SCS                      : positive   := 1;  -- natural?
    G_N_BITS                                  : positive   := 5;
    G_CLK_DIVIDE                              : positive   := 4
    );
  port (
    i_clk                : in  std_ulogic                               := '0';
    i_start              : in  std_ulogic                               := '0';
    o_ready              : out std_ulogic                               := '0';
    o_d_from_peripheral  : out std_ulogic_vector(G_N_BITS - 1 downto 0) := (others => '0');
    i_d_to_peripheral    : in  std_ulogic_vector(G_N_BITS - 1 downto 0) := (others => '0');
    -- setings
    i_sclk_idle_state    :     std_ulogic                               := '1';
    i_scs_idle_state     :     std_ulogic                               := '1';
    -- SPI signals
    o_sclk               : out std_ulogic                               := '1';
    i_sd_from_peripheral : in  std_ulogic                               := '0';
    o_sd_to_peripheral   : out std_ulogic                               := '0';
    o_scs                : out std_ulogic                               := '1'
    );
end entity;

architecture arch of spi_master is

  function divide_by_2_if_even(d : natural) return natural is
  begin
    assert (G_CLK_DIVIDE mod 2) = 0 report "The generic G_CLKDIVIDE needs to be even." severity failure;
    return G_CLK_DIVIDE / 2;
  end function;

  constant C_CLK_DIVIDE         : natural := divide_by_2_if_even(G_CLK_DIVIDE) - 1;
  constant C_N_BITS             : natural := G_N_BITS - 1;
  constant C_N_CLKS_SCLK_TO_SCS : natural := G_N_CLKS_SCLK_TO_SCS;
  constant C_N_CLKS_SCS_TO_SCLK : natural := G_N_CLKS_SCS_TO_SCLK - 1;

  signal counter_n_bits             : natural range 0 to C_N_BITS             := C_N_BITS;
  signal counter_n_clks_sclk_to_scs : natural range 0 to C_N_CLKS_SCLK_TO_SCS := C_N_CLKS_SCLK_TO_SCS;
  signal counter_n_clks_scs_to_sclk : natural range 0 to C_N_CLKS_SCS_TO_SCLK := C_N_CLKS_SCS_TO_SCLK;
  signal counter_clk_divide         : natural range 0 to C_CLK_DIVIDE         := 0;

  signal sclk : std_ulogic := '1';
  signal scs  : std_ulogic := '1';

  signal sclk_edge  : std_ulogic := '0';
  signal sample_sdi : std_ulogic := '0';
  signal sample_sdo : std_ulogic := '0';

  type t_state is (idle, last_edge, trx, wait_sclk, wait_scs);
  signal state : t_state := idle;

  signal ready      : std_ulogic := '0';
  signal reset_sclk : std_ulogic := '1';

begin
  o_ready <= ready;
  o_sclk  <= sclk;
  o_scs   <= scs;

  p_fsm : process(i_clk)
  begin
    if rising_edge(i_clk) then
      case state is
        when wait_sclk =>
          if counter_n_clks_scs_to_sclk = 1 then
            reset_sclk <= '0';
            state      <= trx;
          else
            counter_n_clks_scs_to_sclk <= counter_n_clks_scs_to_sclk - 1;
          end if;

        when trx =>
          if sample_sdo = '1' then
            counter_n_bits <= counter_n_bits - 1;
          end if;

          if counter_n_bits = 0 then
            state <= last_edge;
          end if;

        when last_edge =>
          counter_n_clks_sclk_to_scs <= C_N_CLKS_SCLK_TO_SCS;
          state                      <= wait_scs;

        when wait_scs =>
          reset_sclk <= '1';
          if counter_n_clks_sclk_to_scs = 0 then
            scs   <= i_scs_idle_state;
            ready <= '1';
            state <= idle;
          else
            counter_n_clks_sclk_to_scs <= counter_n_clks_sclk_to_scs - 1;
          end if;

        when others =>                  -- idle
          ready <= '0';
          if i_start = '1' then
            counter_n_clks_scs_to_sclk <= C_N_CLKS_SCS_TO_SCLK;
            scs                        <= not i_scs_idle_state;
            state                      <= wait_sclk;
          end if;
      end case;
    end if;
  end process;

  p_sample_strobes : process(all)
  begin
    if G_TRANSMIT_ON_SCLK_EDGE_TOWARD_IDLE_STATE then
      if i_sclk_idle_state = '1' then
        sample_sdo <= sclk_edge and sclk;
        sample_sdi <= sclk_edge and not sclk;
      else
        sample_sdo <= sclk_edge and not sclk;
        sample_sdi <= sclk_edge and sclk;
      end if;
    else
      if i_sclk_idle_state = '1' then
        sample_sdo <= sclk_edge and not sclk;
        sample_sdi <= sclk_edge and sclk;
      else
        sample_sdo <= sclk_edge and sclk;
        sample_sdi <= sclk_edge and not sclk;
      end if;
    end if;
  end process;

  p_sclk : process(i_clk)
  begin
    if rising_edge(i_clk) then
      if reset_sclk = '1' then
        counter_clk_divide <= 0;
        sclk               <= i_scs_idle_state;
        sclk_edge          <= '0';
      else
        sclk_edge <= '0';
        if counter_clk_divide = 0 then
          counter_clk_divide <= C_CLK_DIVIDE;
          sclk               <= not sclk;
          sclk_edge          <= '1';
        else
          counter_clk_divide <= counter_clk_divide - 1;
        end if;

      --if counter_clk_divide = 1 then
      --  sclk_edge <= '1';
      --end if;
      end if;
    end if;
  end process;

end architecture;

