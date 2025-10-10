onbreak {quit -f}
onerror {quit -f}

vsim -t 1ps -lib xil_defaultlib sram_opt

do {wave.do}

view wave
view structure
view signals

do {sram.udo}

run -all

quit -force
