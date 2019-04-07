package org.apache.commons.text;

import java.text.Format;
import java.util.Locale;

/**
 * Created by root on 4/3/17.
 */
public class LowerCaseFormatFactory implements FormatFactory {
    public static final Format LOWER_INSTANCE = new LowerCaseFormat();
    @Override
    public Format getFormat(final String name, final String arguments, final Locale locale) {
        return LOWER_INSTANCE;
    }
}
