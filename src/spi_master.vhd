library ieee;
use ieee.math_real.all;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity spi_master is
  generic (
    G_N_CLKS_SCS_TO_SCLK   : positive := 1;  -- natural?
    G_N_CLKS_SCLK_TO_SCS   : positive := 1;  -- natural?
    G_MAX_N_BITS_MINUS_1   : natural  := 5;
    G_MAX_SCLK_DIVIDE_HALF : positive := 2
    );
  port (
    i_clk                                     : in  std_ulogic                                                                                := '0';
    i_start                                   : in  std_ulogic                                                                                := '0';
    o_ready                                   : out std_ulogic                                                                                := '0';
    -- data
    i_d_to_peripheral                         : in  std_ulogic_vector(G_MAX_N_BITS_MINUS_1 downto 0)                                          := (others => '0');
    o_d_from_peripheral                       : out std_ulogic_vector(G_MAX_N_BITS_MINUS_1 downto 0)                                          := (others => '0');
    -- setings
    i_n_bits_minus_1                          : in  unsigned(positive(realmax(ceil(log2(real(G_MAX_N_BITS_MINUS_1 + 1))), 1.0)) - 1 downto 0) := (others => '1');
    i_sclk_idle_state                         : in  std_ulogic                                                                                := '1';
    i_sclk_divide_half                        : in  unsigned(positive(ceil(log2(real(G_MAX_SCLK_DIVIDE_HALF + 1)))) - 1 downto 0)             := (others => '1');
    i_scs_idle_state                          : in  std_ulogic                                                                                := '1';
    i_transmit_on_sclk_edge_toward_idle_state : in  std_ulogic                                                                                := '1';
    -- SPI signals
    o_sclk                                    : out std_ulogic                                                                                := '1';
    i_sd_from_peripheral                      : in  std_ulogic                                                                                := '0';
    o_sd_to_peripheral                        : out std_ulogic                                                                                := '0';
    o_scs                                     : out std_ulogic                                                                                := '1'
    );
end entity;

architecture arch of spi_master is

  constant C_N_CLKS_SCLK_TO_SCS : natural := G_N_CLKS_SCLK_TO_SCS;
  constant C_N_CLKS_SCS_TO_SCLK : natural := G_N_CLKS_SCS_TO_SCLK - 1;

  signal counter_n_sclk_edges       : natural range 0 to 2 * (G_MAX_N_BITS_MINUS_1 + 1) - 1 := 2 * (G_MAX_N_BITS_MINUS_1 + 1) - 1;
  signal counter_n_clks_sclk_to_scs : natural range 0 to C_N_CLKS_SCLK_TO_SCS               := C_N_CLKS_SCLK_TO_SCS;
  signal counter_n_clks_scs_to_sclk : natural range 0 to C_N_CLKS_SCS_TO_SCLK               := C_N_CLKS_SCS_TO_SCLK;
  signal counter_clk_divide         : natural range 1 to G_MAX_SCLK_DIVIDE_HALF             := 1;

  signal sclk : std_ulogic := '1';
  signal scs  : std_ulogic := '1';

  signal sclk_edge  : std_ulogic := '0';
  signal sample_sdi : std_ulogic := '0';
  signal sample_sdo : std_ulogic := '0';

  type t_state is (idle, wait_sclk, trx, wait_scs);
  signal state : t_state := idle;

  signal ready      : std_ulogic := '0';
  signal reset_sclk : std_ulogic := '1';

  signal sclk_divide_half                        : natural range 2 to G_MAX_SCLK_DIVIDE_HALF := 2;
  signal transmit_on_sclk_edge_toward_idle_state : std_ulogic                                := '1';

  signal d_to_peripheral   : std_ulogic_vector(G_MAX_N_BITS_MINUS_1 downto 0) := (others => '0');
  signal d_from_peripheral : std_ulogic_vector(G_MAX_N_BITS_MINUS_1 downto 0) := (others => '0');

begin

  p_sample_settings : process(i_clk)
  begin
    if rising_edge(i_clk) then
      if i_start = '1' then
        sclk_divide_half                        <= to_integer(i_sclk_divide_half);
        transmit_on_sclk_edge_toward_idle_state <= i_transmit_on_sclk_edge_toward_idle_state;
      end if;
    end if;
  end process;

  p_output_data_from_peripheral : process(i_clk)
  begin
    if rising_edge(i_clk) then
      if ready = '1' then
        o_d_from_peripheral <= d_from_peripheral;
      end if;
    end if;
  end process;

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
          if sclk_edge = '1' then
            counter_n_sclk_edges <= counter_n_sclk_edges - 1;
          end if;

          if counter_n_sclk_edges = 0 and counter_clk_divide = 1 then
            state                      <= wait_scs;
            counter_n_clks_sclk_to_scs <= C_N_CLKS_SCLK_TO_SCS - 1;
          end if;

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
          scs   <= i_scs_idle_state;  -- make in-port change visible at out-port without the need of a start strobe
          if i_start = '1' then
            counter_n_sclk_edges       <= to_integer(i_n_bits_minus_1 & '0') + 1;
            counter_n_clks_scs_to_sclk <= C_N_CLKS_SCS_TO_SCLK;
            scs                        <= not i_scs_idle_state;
            state                      <= wait_sclk;
          end if;
      end case;
    end if;
  end process;

  p_generate_sample_strobes : process(all)
  begin
    if transmit_on_sclk_edge_toward_idle_state = '1' then
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

  p_generate_sclk : process(i_clk)
  begin
    if rising_edge(i_clk) then
      if reset_sclk = '1' then
        counter_clk_divide <= 1;
        sclk               <= i_sclk_idle_state;
        sclk_edge          <= '0';
      else
        sclk_edge <= '0';
        if counter_clk_divide = 1 then
          counter_clk_divide <= sclk_divide_half;
          sclk               <= not sclk;
          sclk_edge          <= '1';
        else
          counter_clk_divide <= counter_clk_divide - 1;
        end if;
      end if;
    end if;
  end process;

  p_receive_from_peripheral : process(i_clk)
  begin
    if rising_edge(i_clk) then
      if sample_sdi = '1' then
        d_from_peripheral(0)                               <= i_sd_from_peripheral;
        d_from_peripheral(d_from_peripheral'left downto 1) <= d_from_peripheral(d_from_peripheral'left - 1 downto 0);
      end if;
    end if;
  end process;

  p_transmit_to_peripheral : process(i_clk)
  begin
    if rising_edge(i_clk) then
      if i_start = '1' then
        d_to_peripheral <= i_d_to_peripheral;
      else
        o_sd_to_peripheral <= d_to_peripheral(0);
        if sample_sdo = '1' then
          d_to_peripheral(d_to_peripheral'left - 1 downto 0) <= d_to_peripheral(d_to_peripheral'left downto 1);
        end if;
      end if;
    end if;
  end process;

end architecture;

