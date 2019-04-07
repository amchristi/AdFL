package org.apache.commons.text;

import java.text.FieldPosition;
import java.text.Format;
import java.text.ParsePosition;

/**
 * Created by root on 4/3/17.
 */
public  class LowerCaseFormat extends Format {
    private static final long serialVersionUID = 1L;

    @Override
    public StringBuffer format(final Object obj, final StringBuffer toAppendTo, final FieldPosition pos) {
        return toAppendTo.append(((String)obj).toLowerCase());
    }
    @Override
    public Object parseObject(final String source, final ParsePosition pos) {throw new UnsupportedOperationException();}
}
