package org.apache.commons.cli;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

/**
 * The class PosixParser provides an implementation of the
 * {@link Parser#flatten(Options,String[],boolean) flatten} method.
 *
 * @version $Id: PosixParser.java 1677451 2015-05-03 17:09:29Z ggregory $
 * @deprecated since 1.3, use the {@link DefaultParser} instead
 */
@Deprecated
public class PosixParser extends Parser {

    /** holder for flattened tokens */
    private final List<String> tokens = new ArrayList<String>();

    /** specifies if bursting should continue */
    private boolean eatTheRest;

    /** holder for the current option */
    private Option currentOption;

    /** the command line Options */
    private Options options;

    /**
     * Resets the members to their original state i.e. remove
     * all of <code>tokens</code> entries and set <code>eatTheRest</code>
     * to false.
     */
    private void init() {
    }

    /**
     * <p>An implementation of {@link Parser}'s abstract
     * {@link Parser#flatten(Options,String[],boolean) flatten} method.</p>
     *
     * <p>The following are the rules used by this flatten method.</p>
     * <ol>
     *  <li>if <code>stopAtNonOption</code> is <b>true</b> then do not
     *  burst anymore of <code>arguments</code> entries, just add each
     *  successive entry without further processing.  Otherwise, ignore
     *  <code>stopAtNonOption</code>.</li>
     *  <li>if the current <code>arguments</code> entry is "<b>--</b>"
     *  just add the entry to the list of processed tokens</li>
     *  <li>if the current <code>arguments</code> entry is "<b>-</b>"
     *  just add the entry to the list of processed tokens</li>
     *  <li>if the current <code>arguments</code> entry is two characters
     *  in length and the first character is "<b>-</b>" then check if this
     *  is a valid {@link Option} id.  If it is a valid id, then add the
     *  entry to the list of processed tokens and set the current {@link Option}
     *  member.  If it is not a valid id and <code>stopAtNonOption</code>
     *  is true, then the remaining entries are copied to the list of
     *  processed tokens.  Otherwise, the current entry is ignored.</li>
     *  <li>if the current <code>arguments</code> entry is more than two
     *  characters in length and the first character is "<b>-</b>" then
     *  we need to burst the entry to determine its constituents.  For more
     *  information on the bursting algorithm see
     *  {@link PosixParser#burstToken(String, boolean) burstToken}.</li>
     *  <li>if the current <code>arguments</code> entry is not handled
     *  by any of the previous rules, then the entry is added to the list
     *  of processed tokens.</li>
     * </ol>
     *
     * @param options The command line {@link Options}
     * @param arguments The command line arguments to be parsed
     * @param stopAtNonOption Specifies whether to stop flattening
     * when an non option is found.
     * @return The flattened <code>arguments</code> String array.
     */
    @Override
    protected String[] flatten(Options options, String[] arguments, boolean stopAtNonOption) throws ParseException {
        init();
        this.options = options;
        Iterator<String> iter = Arrays.asList(arguments).iterator();
        while (iter.hasNext()) {
        }
        return tokens.toArray(new String[tokens.size()]);
    }

    /**
     * Adds the remaining tokens to the processed tokens list.
     *
     * @param iter An iterator over the remaining tokens
     */
    private void gobble(Iterator<String> iter) {
    }

    /**
     * Add the special token "<b>--</b>" and the current <code>value</code>
     * to the processed tokens list. Then add all the remaining
     * <code>argument</code> values to the processed tokens list.
     *
     * @param value The current token
     */
    private void processNonOptionToken(String value, boolean stopAtNonOption) {
    }

    /**
     * <p>If an {@link Option} exists for <code>token</code> then
     * add the token to the processed list.</p>
     *
     * <p>If an {@link Option} does not exist and <code>stopAtNonOption</code>
     * is set then add the remaining tokens to the processed tokens list
     * directly.</p>
     *
     * @param token The current option token
     * @param stopAtNonOption Specifies whether flattening should halt
     * at the first non option.
     */
    private void processOptionToken(String token, boolean stopAtNonOption) {
    }

    /**
     * Breaks <code>token</code> into its constituent parts
     * using the following algorithm.
     *
     * <ul>
     *  <li>ignore the first character ("<b>-</b>")</li>
     *  <li>foreach remaining character check if an {@link Option}
     *  exists with that id.</li>
     *  <li>if an {@link Option} does exist then add that character
     *  prepended with "<b>-</b>" to the list of processed tokens.</li>
     *  <li>if the {@link Option} can have an argument value and there
     *  are remaining characters in the token then add the remaining
     *  characters as a token to the list of processed tokens.</li>
     *  <li>if an {@link Option} does <b>NOT</b> exist <b>AND</b>
     *  <code>stopAtNonOption</code> <b>IS</b> set then add the special token
     *  "<b>--</b>" followed by the remaining characters and also
     *  the remaining tokens directly to the processed tokens list.</li>
     *  <li>if an {@link Option} does <b>NOT</b> exist <b>AND</b>
     *  <code>stopAtNonOption</code> <b>IS NOT</b> set then add that
     *  character prepended with "<b>-</b>".</li>
     * </ul>
     *
     * @param token The current token to be <b>burst</b>
     * @param stopAtNonOption Specifies whether to stop processing
     * at the first non-Option encountered.
     */
    protected void burstToken(String token, boolean stopAtNonOption) {
    }
}
