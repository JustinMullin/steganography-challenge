## Steganography Challenge

### Usage

`sbt "run encode soundFile textFile outputFile"` encodes [textFile] into [soundFile] and writes the resulting wave to outputFile.
Decoding:
`sbt "run decode soundFile outputFile"` decodes an encoded message from [soundFile] and writes it to [outputFile].
