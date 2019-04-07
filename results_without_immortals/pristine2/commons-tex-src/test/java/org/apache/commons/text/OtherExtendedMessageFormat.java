package org.apache.commons.text;

import java.util.Locale;
import java.util.Map;

/**
 * Created by root on 4/3/17.
 */
public class OtherExtendedMessageFormat extends ExtendedMessageFormat {
    private static final long serialVersionUID = 1L;

    public OtherExtendedMessageFormat(final String pattern, final Locale locale,
                                      final Map<String, ? extends FormatFactory> registry) {
        super(pattern, locale, registry);
    }

}
