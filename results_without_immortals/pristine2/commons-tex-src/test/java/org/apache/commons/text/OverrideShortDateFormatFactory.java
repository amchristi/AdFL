package org.apache.commons.text;

import java.text.DateFormat;
import java.text.Format;
import java.util.Locale;

/**
 * Created by root on 4/3/17.
 */
public  class OverrideShortDateFormatFactory implements FormatFactory {
    @Override
    public Format getFormat(final String name, final String arguments, final Locale locale) {
        return !"short".equals(arguments) ? null
                : locale == null ? DateFormat
                .getDateInstance(DateFormat.DEFAULT) : DateFormat
                .getDateInstance(DateFormat.DEFAULT, locale);
    }
}
