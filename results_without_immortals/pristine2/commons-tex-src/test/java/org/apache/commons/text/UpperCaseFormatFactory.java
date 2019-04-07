package org.apache.commons.text;

import java.text.Format;
import java.util.Locale;

/**
 * Created by root on 4/3/17.
 */
public  class UpperCaseFormatFactory implements FormatFactory {
    private static final Format UPPER_INSTANCE = new UpperCaseFormat();
    @Override
    public Format getFormat(final String name, final String arguments, final Locale locale) {
        return UPPER_INSTANCE;
    }
}
