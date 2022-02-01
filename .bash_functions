# this function uses spark bash script from https://github.com/holman/spark
function sine_wave() {
    SIN=$(python -c "from math import *;print map( lambda x: ceil(6*sin((x+$i)*pi/5)), range($(tput cols)) )" | tr -d '[]' | spark)
    echo -ne $SIN\\r
}
