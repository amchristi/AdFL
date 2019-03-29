SevenZOutputFile
SevenZOutputFile
SevenZOutputFile
SevenZOutputFile
SevenZOutputFile
~~~
setContentCompression
~~~
setContentMethods
~~~
close
~~~
createArchiveEntry
~~~
putArchiveEntry
~~~
closeArchiveEntry
~~~
write
~~~
write
~~~
write
~~~
finish
~~~
getCurrentOutputStream
~~~
setupFileOutputStream
~
if (files.isEmpty()) {
    throw new IllegalStateException("No current 7z entry");
}
~~~
getContentMethods
~~~
writeHeader
~~~
writeStreamsInfo
~~~
writePackInfo
~~~
writeUnpackInfo
~~~
writeFolder
~~~
writeSingleCodec
~~~
writeSubStreamsInfo
~~~
writeFilesInfo
~~~
writeFileEmptyStreams
~~~
writeFileEmptyFiles
~~~
writeFileAntiItems
~~~
writeFileNames
~~~
writeFileCTimes
~
if (numCreationDates > 0) {
    header.write(NID.kCTime);
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    final DataOutputStream out = new DataOutputStream(baos);
    if (numCreationDates != files.size()) {
        out.write(0);
        final BitSet cTimes = new BitSet(files.size());
        for (int i = 0; i < files.size(); i++) {
            cTimes.set(i, files.get(i).getHasCreationDate());
        }
        writeBits(out, cTimes, files.size());
    } else {
        out.write(1);
    }
    out.write(0);
    for (final SevenZArchiveEntry entry : files) {
        if (entry.getHasCreationDate()) {
            out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getCreationDate())));
        }
    }
    out.flush();
    final byte[] contents = baos.toByteArray();
    writeUint64(header, contents.length);
    header.write(contents);
}
~~~
writeFileATimes
~
if (numAccessDates > 0) {
    header.write(NID.kATime);
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    final DataOutputStream out = new DataOutputStream(baos);
    if (numAccessDates != files.size()) {
        out.write(0);
        final BitSet aTimes = new BitSet(files.size());
        for (int i = 0; i < files.size(); i++) {
            aTimes.set(i, files.get(i).getHasAccessDate());
        }
        writeBits(out, aTimes, files.size());
    } else {
        out.write(1);
    }
    out.write(0);
    for (final SevenZArchiveEntry entry : files) {
        if (entry.getHasAccessDate()) {
            out.writeLong(Long.reverseBytes(SevenZArchiveEntry.javaTimeToNtfsTime(entry.getAccessDate())));
        }
    }
    out.flush();
    final byte[] contents = baos.toByteArray();
    writeUint64(header, contents.length);
    header.write(contents);
}
~~~
writeFileMTimes
~~~
writeFileWindowsAttributes
~
if (numWindowsAttributes > 0) {
    header.write(NID.kWinAttributes);
    final ByteArrayOutputStream baos = new ByteArrayOutputStream();
    final DataOutputStream out = new DataOutputStream(baos);
    if (numWindowsAttributes != files.size()) {
        out.write(0);
        final BitSet attributes = new BitSet(files.size());
        for (int i = 0; i < files.size(); i++) {
            attributes.set(i, files.get(i).getHasWindowsAttributes());
        }
        writeBits(out, attributes, files.size());
    } else {
        out.write(1);
    }
    out.write(0);
    for (final SevenZArchiveEntry entry : files) {
        if (entry.getHasWindowsAttributes()) {
            out.writeInt(Integer.reverseBytes(entry.getWindowsAttributes()));
        }
    }
    out.flush();
    final byte[] contents = baos.toByteArray();
    writeUint64(header, contents.length);
    header.write(contents);
}
~~~
writeUint64
~~~
writeBits
~~~
reverse
