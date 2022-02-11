# SI/TI New vs. Legacy

Comparison of new SI/TI vs. legacy definition from P.910.

## Requirements

- FFmpeg
- GNU `parallel`
- AWS CLI tools
- Python 3.8 or higher
- RStudio

SITI-Tools:

```
git clone https://github.com/Telecommunication-Telemedia-Assessment/siti-tools
cd siti-tools
git checkout siti2020
pip3 install .
```

Download the source videos:

```bash
aws s3 cp --no-sign-request s3://download.opencontent.netflix.com/TechblogAssets/Meridian/encodes/Meridian_3840x2160_5994fps_HDR10.mp4 .
aws s3 cp --no-sign-request s3://download.opencontent.netflix.com/TechblogAssets/Meridian/encodes/Meridian_3840x2160_5994fps_SDR.mp4 .
aws s3 cp --no-sign-request s3://download.opencontent.netflix.com/TechblogAssets/CosmosLaundromat/encodes/CosmosLaundromat_2048x858_24fps_HDR10.mp4 .
aws s3 cp --no-sign-request s3://download.opencontent.netflix.com/TechblogAssets/CosmosLaundromat/encodes/CosmosLaundromat_2048x858_24fps_SDR.mp4 .
aws s3 cp --no-sign-request s3://download.opencontent.netflix.com/TechblogAssets/Sparks/encodes/Sparks_4096x2160_5994fps_HDR10.mp4 .
aws s3 cp --no-sign-request s3://download.opencontent.netflix.com/TechblogAssets/Sparks/encodes/Sparks_4096x2160_5994fps_SDR.mp4 .
```

If you want, to run the analyses themselves:

```bash
./calculate-siti.sh
```

## Analysis

Open the `analyze.R` file and source the script.

## License

Copyright 2022 Werner Robitza.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
