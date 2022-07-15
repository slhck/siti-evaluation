# Common Dataset Analysis

Comparison of new SI/TI vs other metrics on joint dataset.

## Requirements

- FFmpeg
- GNU `parallel`
- Python 3.8 or higher
- RStudio

SITI-Tools:

```bash
cd /path/to/where/you/install/software
git clone https://github.com/VQEG/siti-tools
pip3 install .
```

VCA:

```bash
cd /path/to/where/you/install/software
git clone https://github.com/cd-athena/VCA.git
cd VCA
mkdir build
cd build
cmake ../
cmake --build .
```

Download the source video El Fuente (to be supplied in CDVL).

## Calculation

```bash
./calculate_siti.sh /path/to/ElFuente/

# modify as needed
VCAPATH=/path/to/where/you/install/software/VCA/build/source/apps/vca/
PATH=$VCAPATH:$PATH ./calculate_vca.sh /path/to/ElFuente/
```

## Data Analysis

The data committed here is based on:

- siti-tools v0.2.1
- Video Complexity Analyzer v1.5+2-g9f209dc

To analyze it, run `analyze.R`.

## License

Copyright 2022 Werner Robitza.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
