
package org.apache.tools.ant.taskdefs;

import java.io.File;
import java.io.InputStream;

import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.PropertyHelper;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.taskdefs.condition.Condition;
import org.apache.tools.ant.types.EnumeratedAttribute;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.StringUtils;



public class FileDir extends EnumeratedAttribute {

    private static final String[] VALUES = {"file", "dir"};

    /**
     * @see EnumeratedAttribute#getValues
     */
    /** {@inheritDoc}. */
    public String[] getValues() {
        return VALUES;
    }

    /**
     * Indicate if the value specifies a directory.
     *
     * @return true if the value specifies a directory.
     */
    public boolean isDir() {
        return "dir".equalsIgnoreCase(getValue());
    }

    /**
     * Indicate if the value specifies a file.
     *
     * @return true if the value specifies a file.
     */
    public boolean isFile() {
        return "file".equalsIgnoreCase(getValue());
    }

}