#! /bin/bash

HOME_FILES="
src/spi_master.vhd
src/spi_package.vhd
src/simulation/tb_spi_master.vhd
"

for f in $HOME_FILES
do
  vsg --configuration $VSGCONFIG -f "$(pwd)/$f" --fix
  echo "Formatted $f"
done

