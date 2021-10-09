# quad-ci
Implementation of Quad CI as per "The Simple Haskell Handbook" by Marco Sampellegrini

## Development

### Setup
Install `stack`.

Install `record-dot-preprocessor` binary on your machine via `stack install record-dot-preprocessor`.

Install `docker`.

NOTE: If you are using HLS, it might complain it can't execute `record-dot-preprocessor` (rdp). In my case, that was because HSL was installed globally (in `/usr/bin`) while rdp was installed locally (in `/home/martin/.local/bin`), so I fixed it by copying rdp bin to `/usr/bin`. Hacky, but it worked.

### Building
`stack build`
