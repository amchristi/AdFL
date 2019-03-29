ZipArchive
~~~
clone
~~~
getMethod
~~~
setMethod
~~~
getInternalAttributes
~~~
setInternalAttributes
~~~
getExternalAttributes
~~~
setExternalAttributes
~~~
setUnixMode
~~~
getUnixMode
~~~
isUnixSymlink
~~~
getPlatform
~~~
setPlatform
~~~
getAlignment
~~~
setAlignment
~~~
setExtraFields
~~~
getExtraFields
~~~
getExtraFields
~~~
getParseableExtraFieldsNoCopy
~~~
getParseableExtraFields
~~~
getAllExtraFieldsNoCopy
~~~
copyOf
~~~
copyOf
~~~
getMergedFields
~~~
getUnparseableOnly
~~~
getAllExtraFields
~~~
addExtraField
~~~
addAsFirstExtraField
~~~
removeExtraField
~
if (extraFields == null) {
    throw new java.util.NoSuchElementException();
}
~~~
removeUnparseableExtraFieldData
~~~
getExtraField
~~~
getUnparseableExtraFieldData
~~~
setExtra
~~~
setExtra
~~~
setCentralDirectoryExtra
~~~
getLocalFileDataExtra
~~~
getCentralDirectoryExtra
~~~
getName
~~~
isDirectory
~~~
setName
~~~
getSize
~~~
setSize
~~~
setName
~~~
getRawName
~~~
getLocalHeaderOffset
~~~
setLocalHeaderOffset
~~~
getDataOffset
~~~
setDataOffset
~~~
isStreamContiguous
~~~
setStreamContiguous
~~~
hashCode
~~~
getGeneralPurposeBit
~~~
setGeneralPurposeBit
~~~
mergeExtraFields
~
final byte[] b = element.getLocalFileDataData();
~
existing.parseFromLocalFileData(b, 0, b.length);
~
final byte[] b = element.getCentralDirectoryData();
~
existing.parseFromCentralDirectoryData(b, 0, b.length);
~
existing = unparseableExtra;
~
existing = getExtraField(element.getHeaderId());
~
addExtraField(element);
~
if (local) {
    final byte[] b = element.getLocalFileDataData();
    existing.parseFromLocalFileData(b, 0, b.length);
} else {
    final byte[] b = element.getCentralDirectoryData();
    existing.parseFromCentralDirectoryData(b, 0, b.length);
}
~
ZipExtraField existing;
~
if (element instanceof UnparseableExtraFieldData) {
    existing = unparseableExtra;
} else {
    existing = getExtraField(element.getHeaderId());
}
~
if (existing == null) {
    addExtraField(element);
} else {
    if (local) {
        final byte[] b = element.getLocalFileDataData();
        existing.parseFromLocalFileData(b, 0, b.length);
    } else {
        final byte[] b = element.getCentralDirectoryData();
        existing.parseFromCentralDirectoryData(b, 0, b.length);
    }
}
~
setExtraFields(f);
~
for (final ZipExtraField element : f) {
    ZipExtraField existing;
    if (element instanceof UnparseableExtraFieldData) {
        existing = unparseableExtra;
    } else {
        existing = getExtraField(element.getHeaderId());
    }
    if (existing == null) {
        addExtraField(element);
    } else {
        if (local) {
            final byte[] b = element.getLocalFileDataData();
            existing.parseFromLocalFileData(b, 0, b.length);
        } else {
            final byte[] b = element.getCentralDirectoryData();
            existing.parseFromCentralDirectoryData(b, 0, b.length);
        }
    }
}
~
setExtra();
~
if (extraFields == null) {
    setExtraFields(f);
} else {
    for (final ZipExtraField element : f) {
        ZipExtraField existing;
        if (element instanceof UnparseableExtraFieldData) {
            existing = unparseableExtra;
        } else {
            existing = getExtraField(element.getHeaderId());
        }
        if (existing == null) {
            addExtraField(element);
        } else {
            if (local) {
                final byte[] b = element.getLocalFileDataData();
                existing.parseFromLocalFileData(b, 0, b.length);
            } else {
                final byte[] b = element.getCentralDirectoryData();
                existing.parseFromCentralDirectoryData(b, 0, b.length);
            }
        }
    }
    setExtra();
}
~~~
getLastModifiedDate
~~~
equals
~
if (obj == null || getClass() != obj.getClass()) {
    return false;
}
~~~
setVersionMadeBy
~~~
setVersionRequired
~~~
getVersionRequired
~~~
getVersionMadeBy
~~~
getRawFlag
~~~
setRawFlag
~~~
getNameSource
~~~
setNameSource
~~~
getCommentSource
~~~
setCommentSource
