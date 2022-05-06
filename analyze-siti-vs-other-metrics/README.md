# SI/TI vs. other metrics and compressibility

Comparison of SI/TI and other metrics vs. compressibility.

This is a fully reproducible example of the following publication:

Robitza, W., Rao Ramachandra Rao, R., Göring, S., & Raake, A. (2021). Impact of spatial and temporal information on video quality and compressibility. 2021 13th International Conference on Quality of Multimedia Experience, QoMEX 2021, 65–68.

## Requirements

- RStudio

## Data

This uses the [AVT-VQDB-UHD1](https://github.com/Telecommunication-Telemedia-Assessment/AVT-VQDB-UHD-1) database.

The data is already in the `data` directory.

To recreate the data for the CRF-based encoding, you need:

- GNU parallel
- ffmpeg

and run the `./analyze-crf.sh` script.

## Analysis

Open the `siti_analysis.R` file and source the script.

## License

Copyright 2022 Werner Robitza.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
