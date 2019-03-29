Available
~~~
setSearchParents
~~~
setClasspath
~~~
createClasspath
~~~
setClasspathRef
~
createClasspath().setRefid(r);
~~~
setFilepath
~~~
createFilepath
~~~
setProperty
~~~
setValue
~
this.value = value;
~~~
setValue
~
this.value = value;
~~~
setClassname
~~~
setFile
~~~
setResource
~~~
setType
~
log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
~
this.type = new FileDir();
~
this.type.setValue(type);
~~~
setType
~
log("DEPRECATED - The setType(String) method has been deprecated." + " Use setType(Available.FileDir) instead.", Project.MSG_WARN);
~
this.type = new FileDir();
~
this.type.setValue(type);
~~~
setIgnoresystemclasses
~~~
execute
~
throw new BuildException("property attribute is required", getLocation());
~
if (property == null) {
    throw new BuildException("property attribute is required", getLocation());
}
~
isTask = true;
~
try {
    if (eval()) {
        PropertyHelper ph = PropertyHelper.getPropertyHelper(getProject());
        Object oldvalue = ph.getProperty(property);
        if (null != oldvalue && !oldvalue.equals(value)) {
            log("DEPRECATED - <available> used to override an existing" + " property." + StringUtils.LINE_SEP + "  Build file should not reuse the same property" + " name for different values.", Project.MSG_WARN);
        }
        // due to backwards compatibility reasons
        ph.setProperty(property, value, true);
    }
} finally {
    isTask = false;
}
~~~
eval
~~~
checkFile
~
log("Found: " + path, Project.MSG_VERBOSE);
~
if (type.isDir() && path.isDirectory()) {
    log("Found directory: " + path, Project.MSG_VERBOSE);
    return true;
} else if (type.isFile() && path.isFile()) {
    log("Found file: " + path, Project.MSG_VERBOSE);
    return true;
}
~
log("Found: " + parent, Project.MSG_VERBOSE);
~
if (type.isDir()) {
    log("Found directory: " + parent, Project.MSG_VERBOSE);
    return true;
}
~
return true;
~
return true;
~
if (type == null) {
    log("Found: " + path, Project.MSG_VERBOSE);
    return true;
} else if (type.isDir() && path.isDirectory()) {
    log("Found directory: " + path, Project.MSG_VERBOSE);
    return true;
} else if (type.isFile() && path.isFile()) {
    log("Found file: " + path, Project.MSG_VERBOSE);
    return true;
}
~
// not the requested type
return false;
~
if (type == null) {
    log("Found: " + parent, Project.MSG_VERBOSE);
    return true;
} else if (type.isDir()) {
    log("Found directory: " + parent, Project.MSG_VERBOSE);
    return true;
}
~
// not the requested type
return false;
~
if (checkFile(new File(path, filename), filename + " in " + path)) {
    return true;
}
~
if (checkFile(new File(parent, filename), filename + " in " + parent)) {
    return true;
}
~
parent = parent.getParentFile();
~~~
checkFile
~
log("Found: " + path, Project.MSG_VERBOSE);
~
if (type.isDir() && path.isDirectory()) {
    log("Found directory: " + path, Project.MSG_VERBOSE);
    return true;
} else if (type.isFile() && path.isFile()) {
    log("Found file: " + path, Project.MSG_VERBOSE);
    return true;
}
~
log("Found: " + parent, Project.MSG_VERBOSE);
~
if (type.isDir()) {
    log("Found directory: " + parent, Project.MSG_VERBOSE);
    return true;
}
~
return true;
~
return true;
~
if (type == null) {
    log("Found: " + path, Project.MSG_VERBOSE);
    return true;
} else if (type.isDir() && path.isDirectory()) {
    log("Found directory: " + path, Project.MSG_VERBOSE);
    return true;
} else if (type.isFile() && path.isFile()) {
    log("Found file: " + path, Project.MSG_VERBOSE);
    return true;
}
~
// not the requested type
return false;
~
if (type == null) {
    log("Found: " + parent, Project.MSG_VERBOSE);
    return true;
} else if (type.isDir()) {
    log("Found directory: " + parent, Project.MSG_VERBOSE);
    return true;
}
~
// not the requested type
return false;
~
if (checkFile(new File(path, filename), filename + " in " + path)) {
    return true;
}
~
if (checkFile(new File(parent, filename), filename + " in " + parent)) {
    return true;
}
~
parent = parent.getParentFile();
~~~
checkResource
~~~
checkClass
