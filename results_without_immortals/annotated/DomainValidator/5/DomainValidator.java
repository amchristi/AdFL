/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.validator.routines;

import java.io.Serializable;
import java.net.IDN;
import java.util.Arrays;
import java.util.Locale;
import java.io.*;

/**
 * <p><b>Domain name</b> validation routines.</p>
 *
 * <p>
 * This validator provides methods for validating Internet domain names
 * and top-level domains.
 * </p>
 *
 * <p>Domain names are evaluated according
 * to the standards <a href="http://www.ietf.org/rfc/rfc1034.txt">RFC1034</a>,
 * section 3, and <a href="http://www.ietf.org/rfc/rfc1123.txt">RFC1123</a>,
 * section 2.1. No accommodation is provided for the specialized needs of
 * other applications; if the domain name has been URL-encoded, for example,
 * validation will fail even though the equivalent plaintext version of the
 * same name would have passed.
 * </p>
 *
 * <p>
 * Validation is also provided for top-level domains (TLDs) as defined and
 * maintained by the Internet Assigned Numbers Authority (IANA):
 * </p>
 *
 * <ul>
 * <li>{@link #isValidInfrastructureTld} - validates infrastructure TLDs
 * (<code>.arpa</code>, etc.)</li>
 * <li>{@link #isValidGenericTld} - validates generic TLDs
 * (<code>.com, .org</code>, etc.)</li>
 * <li>{@link #isValidCountryCodeTld} - validates country code TLDs
 * (<code>.us, .uk, .cn</code>, etc.)</li>
 * </ul>
 *
 * <p>
 * (<b>NOTE</b>: This class does not provide IP address lookup for domain names or
 * methods to ensure that a given domain name matches a specific IP; see
 * {@link java.net.InetAddress} for that functionality.)
 * </p>
 *
 * @version $Revision: 1713225 $
 * @since Validator 1.4
 */
public class DomainValidator implements Serializable {

    private static final String[] EMPTY_STRING_ARRAY = new String[0];

    private static final long serialVersionUID = -4407125112880174009L;

    // Regular expression strings for hostnames (derived from RFC2396 and RFC 1123)
    // RFC2396: domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
    // Max 63 characters
    private static final String DOMAIN_LABEL_REGEX = "\\p{Alnum}(?>[\\p{Alnum}-]{0,61}\\p{Alnum})?";

    // RFC2396 toplabel = alpha | alpha *( alphanum | "-" ) alphanum
    // Max 63 characters
    private static final String TOP_LABEL_REGEX = "\\p{Alpha}(?>[\\p{Alnum}-]{0,61}\\p{Alnum})?";

    // RFC2396 hostname = *( domainlabel "." ) toplabel [ "." ]
    // Note that the regex currently requires both a domain label and a top level label, whereas
    // the RFC does not. This is because the regex is used to detect if a TLD is present.
    // If the match fails, input is checked against DOMAIN_LABEL_REGEX (hostnameRegex)
    // RFC1123 sec 2.1 allows hostnames to start with a digit
    private static final String DOMAIN_NAME_REGEX = "^(?:" + DOMAIN_LABEL_REGEX + "\\.)+" + "(" + TOP_LABEL_REGEX + ")\\.?$";

    private final boolean allowLocal;

    /**
     * Singleton instance of this validator, which
     * doesn't consider local addresses as valid.
     */
    private static final DomainValidator DOMAIN_VALIDATOR = new DomainValidator(false);

    /**
     * Singleton instance of this validator, which does
     * consider local addresses valid.
     */
    private static final DomainValidator DOMAIN_VALIDATOR_WITH_LOCAL = new DomainValidator(true);

    /**
     * RegexValidator for matching domains.
     */
    private final RegexValidator domainRegex = new RegexValidator(DOMAIN_NAME_REGEX);

    /**
     * RegexValidator for matching a local hostname
     */
    // RFC1123 sec 2.1 allows hostnames to start with a digit
    private final RegexValidator hostnameRegex = new RegexValidator(DOMAIN_LABEL_REGEX);

    /**
     * Returns the singleton instance of this validator. It
     * will not consider local addresses as valid.
     * @return the singleton instance of this validator
     */
    public static synchronized DomainValidator getInstance() {
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "e17cf455-bcd1-4640-ad1e-e57ff66535b7");
        inUse = true;
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "5cf2075e-7f68-4e87-bc0a-049b71ef8e39");
        return DOMAIN_VALIDATOR;
    }

    /**
     * Returns the singleton instance of this validator,
     * with local validation as required.
     * @param allowLocal Should local addresses be considered valid?
     * @return the singleton instance of this validator
     */
    public static synchronized DomainValidator getInstance(boolean allowLocal) {
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "8ebd4073-19a8-459a-8e8d-1b368d7236cf");
        inUse = true;
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "0f541969-1f17-4fad-8f4a-77c924ef9fdc");
        if (allowLocal) {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "50c4d0a9-0046-454d-98b8-ceac2285b8a1");
            return DOMAIN_VALIDATOR_WITH_LOCAL;
        }
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "6cdebae1-915f-46dd-9f81-178297b9cf7c");
        return DOMAIN_VALIDATOR;
    }

    /**
     * Private constructor.
     */
    private DomainValidator(boolean allowLocal) {
        this.allowLocal = allowLocal;
    }

    /**
     * Returns true if the specified <code>String</code> parses
     * as a valid domain name with a recognized top-level domain.
     * The parsing is case-insensitive.
     * @param domain the parameter to check for domain name syntax
     * @return true if the parameter is a valid domain name
     */
    public boolean isValid(String domain) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "aaacc159-47d8-48d4-bb59-cc92e2dc6937");
        if (domain == null) {
            writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "625d8a87-9968-4c6a-9ad4-efbf6f3cec98");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "12da40c2-fdf7-4d8d-96f8-0a8588058599");
        domain = unicodeToASCII(domain);
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "f82b28e2-1e0f-4efd-b205-27b87de2b28c");
        // checks in the regexes below
        if (domain.length() > 253) {
            writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "0c9d4aa4-170e-4cbe-9056-e8e06288a160");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "fb9360d9-ab91-45e3-a5cb-5bad1dfc0cc2");
        String[] groups = domainRegex.match(domain);
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "af9a3702-f5da-4d65-9e88-27f4247ce1be");
        if (groups != null && groups.length > 0) {
            writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "468208b0-7d76-41e9-bfae-ed33a1bd7769");
            return isValidTld(groups[0]);
        }
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "5d767a0f-9ae7-4c6f-ad8c-69d26d109b26");
        return allowLocal && hostnameRegex.isValid(domain);
    }

    // package protected for unit test access
    // must agree with isValid() above
    final boolean isValidDomainSyntax(String domain) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "42da62e1-0e42-49a1-8632-02a5aa274f46");
        if (domain == null) {
            writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "5304db4f-2648-4d22-b7c2-bc8c8141bfef");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "e0703354-68d9-471d-9299-3a901f9a95e3");
        domain = unicodeToASCII(domain);
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "4cefa4d5-62f2-43cb-8b9e-3d795303c0d1");
        // checks in the regexes below
        if (domain.length() > 253) {
            writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "926c9d99-7058-4b0d-a89b-bdf0c40dab42");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "d7d95e31-9358-4227-be19-6a12e3ebfac6");
        String[] groups = domainRegex.match(domain);
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "86191442-3211-4d4e-9e24-0060f9470ea5");
        return (groups != null && groups.length > 0) || hostnameRegex.isValid(domain);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined top-level domain. Leading dots are ignored if present.
     * The search is case-insensitive.
     * @param tld the parameter to check for TLD status, not null
     * @return true if the parameter is a TLD
     */
    public boolean isValidTld(String tld) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "b3a81274-8154-49d0-9b44-97b161199a0c");
        tld = unicodeToASCII(tld);
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "cbe85758-c745-4697-9454-c189d62f0bf1");
        if (allowLocal && isValidLocalTld(tld)) {
            writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "b6638160-f805-42ba-8b49-ef49bb1aee79");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "c8ba51cf-7b38-4234-8d7b-eb7d662ef090");
        return isValidInfrastructureTld(tld) || isValidGenericTld(tld) || isValidCountryCodeTld(tld);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined infrastructure top-level domain. Leading dots are
     * ignored if present. The search is case-insensitive.
     * @param iTld the parameter to check for infrastructure TLD status, not null
     * @return true if the parameter is an infrastructure TLD
     */
    public boolean isValidInfrastructureTld(String iTld) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "b5837d0b-f635-466d-a22a-81afa27303d8");
        final String key = chompLeadingDot(unicodeToASCII(iTld).toLowerCase(Locale.ENGLISH));
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "8e4ff94e-e8ed-4e4c-b378-1c0ef0bd7819");
        return arrayContains(INFRASTRUCTURE_TLDS, key);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined generic top-level domain. Leading dots are ignored
     * if present. The search is case-insensitive.
     * @param gTld the parameter to check for generic TLD status, not null
     * @return true if the parameter is a generic TLD
     */
    public boolean isValidGenericTld(String gTld) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "1e7b9c56-0a3c-4b91-bf94-f1f6fb04d14f");
        final String key = chompLeadingDot(unicodeToASCII(gTld).toLowerCase(Locale.ENGLISH));
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "f3932eaf-0a2b-4615-aaae-2721ba05e67b");
        return (arrayContains(GENERIC_TLDS, key) || arrayContains(GENERIC_TLDS_PLUS, key)) && !arrayContains(GENERIC_TLDS_MINUS, key);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined country code top-level domain. Leading dots are
     * ignored if present. The search is case-insensitive.
     * @param ccTld the parameter to check for country code TLD status, not null
     * @return true if the parameter is a country code TLD
     */
    public boolean isValidCountryCodeTld(String ccTld) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "da4b693d-2fa9-4c02-9e01-b81ef28bc6a5");
        final String key = chompLeadingDot(unicodeToASCII(ccTld).toLowerCase(Locale.ENGLISH));
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "7f3aff0f-6b12-4187-99b6-8a600930e2fa");
        return (arrayContains(COUNTRY_CODE_TLDS, key) || arrayContains(COUNTRY_CODE_TLDS_PLUS, key)) && !arrayContains(COUNTRY_CODE_TLDS_MINUS, key);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * widely used "local" domains (localhost or localdomain). Leading dots are
     * ignored if present. The search is case-insensitive.
     * @param lTld the parameter to check for local TLD status, not null
     * @return true if the parameter is an local TLD
     */
    public boolean isValidLocalTld(String lTld) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "cf1377a4-f7f7-4215-96b4-380283d52a37");
        final String key = chompLeadingDot(unicodeToASCII(lTld).toLowerCase(Locale.ENGLISH));
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "98926d9e-990b-4219-8804-372c410625f4");
        return arrayContains(LOCAL_TLDS, key);
    }

    private String chompLeadingDot(String str) {
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "75bf0bf8-3186-41ef-a230-a640e9e4e9c6");
        if (str.startsWith(".")) {
            writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "cda7d0e8-b3a4-42f7-8bda-2f01609e469a");
            return str.substring(1);
        }
        writeline("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "cc6c27b9-3505-4ed0-bc47-c4ad7101ea26");
        return str;
    }

    // ---------------------------------------------
    // ----- TLDs defined by IANA
    // ----- Authoritative and comprehensive list at:
    // ----- http://data.iana.org/TLD/tlds-alpha-by-domain.txt
    // Note that the above list is in UPPER case.
    // The code currently converts strings to lower case (as per the tables below)
    // IANA also provide an HTML list at http://www.iana.org/domains/root/db
    // Note that this contains several country code entries which are NOT in
    // the text file. These all have the "Not assigned" in the "Sponsoring Organisation" column
    // For example (as of 2015-01-02):
    // .bl  country-code    Not assigned
    // .um  country-code    Not assigned
    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static final String[] INFRASTRUCTURE_TLDS = new String[] { // internet infrastructure
    "arpa" };

    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static final String[] GENERIC_TLDS = new String[] { // aaa American Automobile Association, Inc.
    "aaa", // aarp AARP
    "aarp", // abb ABB Ltd
    "abb", // abbott Abbott Laboratories, Inc.
    "abbott", // abogado Top Level Domain Holdings Limited
    "abogado", // academy Half Oaks, LLC
    "academy", // accenture Accenture plc
    "accenture", // accountant dot Accountant Limited
    "accountant", // accountants Knob Town, LLC
    "accountants", // aco ACO Severin Ahlmann GmbH &amp; Co. KG
    "aco", // active The Active Network, Inc
    "active", // actor United TLD Holdco Ltd.
    "actor", // ads Charleston Road Registry Inc.
    "ads", // adult ICM Registry AD LLC
    "adult", // aeg Aktiebolaget Electrolux
    "aeg", // aero Societe Internationale de Telecommunications Aeronautique (SITA INC USA)
    "aero", // afl Australian Football League
    "afl", // agency Steel Falls, LLC
    "agency", // aig American International Group, Inc.
    "aig", // airforce United TLD Holdco Ltd.
    "airforce", // airtel Bharti Airtel Limited
    "airtel", // allfinanz Allfinanz Deutsche Vermögensberatung Aktiengesellschaft
    "allfinanz", // alsace REGION D ALSACE
    "alsace", // amica Amica Mutual Insurance Company
    "amica", // amsterdam Gemeente Amsterdam
    "amsterdam", // android Charleston Road Registry Inc.
    "android", // apartments June Maple, LLC
    "apartments", // app Charleston Road Registry Inc.
    "app", // apple Apple Inc.
    "apple", // aquarelle Aquarelle.com
    "aquarelle", // aramco Aramco Services Company
    "aramco", // archi STARTING DOT LIMITED
    "archi", // army United TLD Holdco Ltd.
    "army", // arte Association Relative à la Télévision Européenne G.E.I.E.
    "arte", // asia DotAsia Organisation Ltd.
    "asia", // associates Baxter Hill, LLC
    "associates", // attorney United TLD Holdco, Ltd
    "attorney", // auction United TLD HoldCo, Ltd.
    "auction", // audio Uniregistry, Corp.
    "audio", // auto Uniregistry, Corp.
    "auto", // autos DERAutos, LLC
    "autos", // axa AXA SA
    "axa", // azure Microsoft Corporation
    "azure", // band United TLD Holdco, Ltd
    "band", // bank fTLD Registry Services, LLC
    "bank", // bar Punto 2012 Sociedad Anonima Promotora de Inversion de Capital Variable
    "bar", // barcelona Municipi de Barcelona
    "barcelona", // barclaycard Barclays Bank PLC
    "barclaycard", // barclays Barclays Bank PLC
    "barclays", // bargains Half Hallow, LLC
    "bargains", // bauhaus Werkhaus GmbH
    "bauhaus", // bayern Bayern Connect GmbH
    "bayern", // bbc British Broadcasting Corporation
    "bbc", // bbva BANCO BILBAO VIZCAYA ARGENTARIA, S.A.
    "bbva", // bcn Municipi de Barcelona
    "bcn", // beats Beats Electronics, LLC
    "beats", // beer Top Level Domain Holdings Limited
    "beer", // bentley Bentley Motors Limited
    "bentley", // berlin dotBERLIN GmbH &amp; Co. KG
    "berlin", // best BestTLD Pty Ltd
    "best", // bet Afilias plc
    "bet", // bharti Bharti Enterprises (Holding) Private Limited
    "bharti", // bible American Bible Society
    "bible", // bid dot Bid Limited
    "bid", // bike Grand Hollow, LLC
    "bike", // bing Microsoft Corporation
    "bing", // bingo Sand Cedar, LLC
    "bingo", // bio STARTING DOT LIMITED
    "bio", // biz Neustar, Inc.
    "biz", // black Afilias Limited
    "black", // blackfriday Uniregistry, Corp.
    "blackfriday", // bloomberg Bloomberg IP Holdings LLC
    "bloomberg", // blue Afilias Limited
    "blue", // bms Bristol-Myers Squibb Company
    "bms", // bmw Bayerische Motoren Werke Aktiengesellschaft
    "bmw", // bnl Banca Nazionale del Lavoro
    "bnl", // bnpparibas BNP Paribas
    "bnpparibas", // boats DERBoats, LLC
    "boats", // bom Núcleo de Informação e Coordenação do Ponto BR - NIC.br
    "bom", // bond Bond University Limited
    "bond", // boo Charleston Road Registry Inc.
    "boo", // boots THE BOOTS COMPANY PLC
    "boots", // boutique Over Galley, LLC
    "boutique", // bradesco Banco Bradesco S.A.
    "bradesco", // bridgestone Bridgestone Corporation
    "bridgestone", // broker DOTBROKER REGISTRY LTD
    "broker", // brother Brother Industries, Ltd.
    "brother", // brussels DNS.be vzw
    "brussels", // budapest Top Level Domain Holdings Limited
    "budapest", // build Plan Bee LLC
    "build", // builders Atomic Madison, LLC
    "builders", // business Spring Cross, LLC
    "business", // buzz DOTSTRATEGY CO.
    "buzz", // bzh Association www.bzh
    "bzh", // cab Half Sunset, LLC
    "cab", // cafe Pioneer Canyon, LLC
    "cafe", // cal Charleston Road Registry Inc.
    "cal", // camera Atomic Maple, LLC
    "camera", // camp Delta Dynamite, LLC
    "camp", // cancerresearch Australian Cancer Research Foundation
    "cancerresearch", // canon Canon Inc.
    "canon", // capetown ZA Central Registry NPC trading as ZA Central Registry
    "capetown", // capital Delta Mill, LLC
    "capital", // car Cars Registry Limited
    "car", // caravan Caravan International, Inc.
    "caravan", // cards Foggy Hollow, LLC
    "cards", // care Goose Cross, LLC
    "care", // career dotCareer LLC
    "career", // careers Wild Corner, LLC
    "careers", // cars Uniregistry, Corp.
    "cars", // cartier Richemont DNS Inc.
    "cartier", // casa Top Level Domain Holdings Limited
    "casa", // cash Delta Lake, LLC
    "cash", // casino Binky Sky, LLC
    "casino", // cat Fundacio puntCAT
    "cat", // catering New Falls. LLC
    "catering", // cba COMMONWEALTH BANK OF AUSTRALIA
    "cba", // cbn The Christian Broadcasting Network, Inc.
    "cbn", // ceb The Corporate Executive Board Company
    "ceb", // center Tin Mill, LLC
    "center", // ceo CEOTLD Pty Ltd
    "ceo", // cern European Organization for Nuclear Research (&quot;CERN&quot;)
    "cern", // cfa CFA Institute
    "cfa", // cfd DOTCFD REGISTRY LTD
    "cfd", // chanel Chanel International B.V.
    "chanel", // channel Charleston Road Registry Inc.
    "channel", // chat Sand Fields, LLC
    "chat", // cheap Sand Cover, LLC
    "cheap", // chloe Richemont DNS Inc.
    "chloe", // christmas Uniregistry, Corp.
    "christmas", // chrome Charleston Road Registry Inc.
    "chrome", // church Holly Fileds, LLC
    "church", // cipriani Hotel Cipriani Srl
    "cipriani", // cisco Cisco Technology, Inc.
    "cisco", // citic CITIC Group Corporation
    "citic", // city Snow Sky, LLC
    "city", // claims Black Corner, LLC
    "claims", // cleaning Fox Shadow, LLC
    "cleaning", // click Uniregistry, Corp.
    "click", // clinic Goose Park, LLC
    "clinic", // clothing Steel Lake, LLC
    "clothing", // cloud ARUBA S.p.A.
    "cloud", // club .CLUB DOMAINS, LLC
    "club", // clubmed Club Méditerranée S.A.
    "clubmed", // coach Koko Island, LLC
    "coach", // codes Puff Willow, LLC
    "codes", // coffee Trixy Cover, LLC
    "coffee", // college XYZ.COM LLC
    "college", // cologne NetCologne Gesellschaft für Telekommunikation mbH
    "cologne", // com VeriSign Global Registry Services
    "com", // commbank COMMONWEALTH BANK OF AUSTRALIA
    "commbank", // community Fox Orchard, LLC
    "community", // company Silver Avenue, LLC
    "company", // computer Pine Mill, LLC
    "computer", // condos Pine House, LLC
    "condos", // construction Fox Dynamite, LLC
    "construction", // consulting United TLD Holdco, LTD.
    "consulting", // contractors Magic Woods, LLC
    "contractors", // cooking Top Level Domain Holdings Limited
    "cooking", // cool Koko Lake, LLC
    "cool", // coop DotCooperation LLC
    "coop", // corsica Collectivité Territoriale de Corse
    "corsica", // country Top Level Domain Holdings Limited
    "country", // coupons Black Island, LLC
    "coupons", // courses OPEN UNIVERSITIES AUSTRALIA PTY LTD
    "courses", // credit Snow Shadow, LLC
    "credit", // creditcard Binky Frostbite, LLC
    "creditcard", // cricket dot Cricket Limited
    "cricket", // crown Crown Equipment Corporation
    "crown", // crs Federated Co-operatives Limited
    "crs", // cruises Spring Way, LLC
    "cruises", // csc Alliance-One Services, Inc.
    "csc", // cuisinella SALM S.A.S.
    "cuisinella", // cymru Nominet UK
    "cymru", // cyou Beijing Gamease Age Digital Technology Co., Ltd.
    "cyou", // dabur Dabur India Limited
    "dabur", // dad Charleston Road Registry Inc.
    "dad", // dance United TLD Holdco Ltd.
    "dance", // date dot Date Limited
    "date", // dating Pine Fest, LLC
    "dating", // datsun NISSAN MOTOR CO., LTD.
    "datsun", // day Charleston Road Registry Inc.
    "day", // dclk Charleston Road Registry Inc.
    "dclk", // deals Sand Sunset, LLC
    "deals", // degree United TLD Holdco, Ltd
    "degree", // delivery Steel Station, LLC
    "delivery", // dell Dell Inc.
    "dell", // delta Delta Air Lines, Inc.
    "delta", // democrat United TLD Holdco Ltd.
    "democrat", // dental Tin Birch, LLC
    "dental", // dentist United TLD Holdco, Ltd
    "dentist", // desi Desi Networks LLC
    "desi", // design Top Level Design, LLC
    "design", // dev Charleston Road Registry Inc.
    "dev", // diamonds John Edge, LLC
    "diamonds", // diet Uniregistry, Corp.
    "diet", // digital Dash Park, LLC
    "digital", // direct Half Trail, LLC
    "direct", // directory Extra Madison, LLC
    "directory", // discount Holly Hill, LLC
    "discount", // dnp Dai Nippon Printing Co., Ltd.
    "dnp", // docs Charleston Road Registry Inc.
    "docs", // dog Koko Mill, LLC
    "dog", // doha Communications Regulatory Authority (CRA)
    "doha", // domains Sugar Cross, LLC
    "domains", // doosan Doosan Corporation
    "doosan", // download dot Support Limited
    "download", // drive Charleston Road Registry Inc.
    "drive", // durban ZA Central Registry NPC trading as ZA Central Registry
    "durban", // dvag Deutsche Vermögensberatung Aktiengesellschaft DVAG
    "dvag", // earth Interlink Co., Ltd.
    "earth", // eat Charleston Road Registry Inc.
    "eat", // edu EDUCAUSE
    "edu", // education Brice Way, LLC
    "education", // email Spring Madison, LLC
    "email", // emerck Merck KGaA
    "emerck", // energy Binky Birch, LLC
    "energy", // engineer United TLD Holdco Ltd.
    "engineer", // engineering Romeo Canyon
    "engineering", // enterprises Snow Oaks, LLC
    "enterprises", // epson Seiko Epson Corporation
    "epson", // equipment Corn Station, LLC
    "equipment", // erni ERNI Group Holding AG
    "erni", // esq Charleston Road Registry Inc.
    "esq", // estate Trixy Park, LLC
    "estate", // eurovision European Broadcasting Union (EBU)
    "eurovision", // eus Puntueus Fundazioa
    "eus", // events Pioneer Maple, LLC
    "events", // everbank EverBank
    "everbank", // exchange Spring Falls, LLC
    "exchange", // expert Magic Pass, LLC
    "expert", // exposed Victor Beach, LLC
    "exposed", // express Sea Sunset, LLC
    "express", // fage Fage International S.A.
    "fage", // fail Atomic Pipe, LLC
    "fail", // faith dot Faith Limited
    "faith", // family United TLD Holdco Ltd.
    "family", // fan Asiamix Digital Ltd
    "fan", // fans Asiamix Digital Limited
    "fans", // farm Just Maple, LLC
    "farm", // fashion Top Level Domain Holdings Limited
    "fashion", // feedback Top Level Spectrum, Inc.
    "feedback", // ferrero Ferrero Trading Lux S.A.
    "ferrero", // film Motion Picture Domain Registry Pty Ltd
    "film", // final Núcleo de Informação e Coordenação do Ponto BR - NIC.br
    "final", // finance Cotton Cypress, LLC
    "finance", // financial Just Cover, LLC
    "financial", // firmdale Firmdale Holdings Limited
    "firmdale", // fish Fox Woods, LLC
    "fish", // fishing Top Level Domain Holdings Limited
    "fishing", // fit Minds + Machines Group Limited
    "fit", // fitness Brice Orchard, LLC
    "fitness", // flights Fox Station, LLC
    "flights", // florist Half Cypress, LLC
    "florist", // flowers Uniregistry, Corp.
    "flowers", // flsmidth FLSmidth A/S
    "flsmidth", // fly Charleston Road Registry Inc.
    "fly", // foo Charleston Road Registry Inc.
    "foo", // football Foggy Farms, LLC
    "football", // forex DOTFOREX REGISTRY LTD
    "forex", // forsale United TLD Holdco, LLC
    "forsale", // forum Fegistry, LLC
    "forum", // foundation John Dale, LLC
    "foundation", // frl FRLregistry B.V.
    "frl", // frogans OP3FT
    "frogans", // fund John Castle, LLC
    "fund", // furniture Lone Fields, LLC
    "furniture", // futbol United TLD Holdco, Ltd.
    "futbol", // fyi Silver Tigers, LLC
    "fyi", // gal Asociación puntoGAL
    "gal", // gallery Sugar House, LLC
    "gallery", // game Uniregistry, Corp.
    "game", // garden Top Level Domain Holdings Limited
    "garden", // gbiz Charleston Road Registry Inc.
    "gbiz", // gdn Joint Stock Company "Navigation-information systems"
    "gdn", // gea GEA Group Aktiengesellschaft
    "gea", // gent COMBELL GROUP NV/SA
    "gent", // genting Resorts World Inc. Pte. Ltd.
    "genting", // ggee GMO Internet, Inc.
    "ggee", // gift Uniregistry, Corp.
    "gift", // gifts Goose Sky, LLC
    "gifts", // gives United TLD Holdco Ltd.
    "gives", // giving Giving Limited
    "giving", // glass Black Cover, LLC
    "glass", // gle Charleston Road Registry Inc.
    "gle", // global Dot Global Domain Registry Limited
    "global", // globo Globo Comunicação e Participações S.A
    "globo", // gmail Charleston Road Registry Inc.
    "gmail", // gmo GMO Internet, Inc.
    "gmo", // gmx 1&amp;1 Mail &amp; Media GmbH
    "gmx", // gold June Edge, LLC
    "gold", // goldpoint YODOBASHI CAMERA CO.,LTD.
    "goldpoint", // golf Lone Falls, LLC
    "golf", // goo NTT Resonant Inc.
    "goo", // goog Charleston Road Registry Inc.
    "goog", // google Charleston Road Registry Inc.
    "google", // gop Republican State Leadership Committee, Inc.
    "gop", // gov General Services Administration Attn: QTDC, 2E08 (.gov Domain Registration)
    "gov", // graphics Over Madison, LLC
    "graphics", // gratis Pioneer Tigers, LLC
    "gratis", // green Afilias Limited
    "green", // gripe Corn Sunset, LLC
    "gripe", // group Romeo Town, LLC
    "group", // gucci Guccio Gucci S.p.a.
    "gucci", // guge Charleston Road Registry Inc.
    "guge", // guide Snow Moon, LLC
    "guide", // guitars Uniregistry, Corp.
    "guitars", // guru Pioneer Cypress, LLC
    "guru", // hamburg Hamburg Top-Level-Domain GmbH
    "hamburg", // hangout Charleston Road Registry Inc.
    "hangout", // haus United TLD Holdco, LTD.
    "haus", // healthcare Silver Glen, LLC
    "healthcare", // help Uniregistry, Corp.
    "help", // here Charleston Road Registry Inc.
    "here", // hermes Hermes International
    "hermes", // hiphop Uniregistry, Corp.
    "hiphop", // hitachi Hitachi, Ltd.
    "hitachi", // hiv dotHIV gemeinnuetziger e.V.
    "hiv", // hockey Half Willow, LLC
    "hockey", // holdings John Madison, LLC
    "holdings", // holiday Goose Woods, LLC
    "holiday", // homedepot Homer TLC, Inc.
    "homedepot", // homes DERHomes, LLC
    "homes", // honda Honda Motor Co., Ltd.
    "honda", // horse Top Level Domain Holdings Limited
    "horse", // host DotHost Inc.
    "host", // hosting Uniregistry, Corp.
    "hosting", // hoteles Travel Reservations SRL
    "hoteles", // hotmail Microsoft Corporation
    "hotmail", // house Sugar Park, LLC
    "house", // how Charleston Road Registry Inc.
    "how", // hsbc HSBC Holdings PLC
    "hsbc", // hyundai Hyundai Motor Company
    "hyundai", // ibm International Business Machines Corporation
    "ibm", // icbc Industrial and Commercial Bank of China Limited
    "icbc", // ice IntercontinentalExchange, Inc.
    "ice", // icu One.com A/S
    "icu", // ifm ifm electronic gmbh
    "ifm", // iinet Connect West Pty. Ltd.
    "iinet", // immo Auburn Bloom, LLC
    "immo", // immobilien United TLD Holdco Ltd.
    "immobilien", // industries Outer House, LLC
    "industries", // infiniti NISSAN MOTOR CO., LTD.
    "infiniti", // info Afilias Limited
    "info", // ing Charleston Road Registry Inc.
    "ing", // ink Top Level Design, LLC
    "ink", // institute Outer Maple, LLC
    "institute", // insure Pioneer Willow, LLC
    "insure", // int Internet Assigned Numbers Authority
    "int", // international Wild Way, LLC
    "international", // investments Holly Glen, LLC
    "investments", // ipiranga Ipiranga Produtos de Petroleo S.A.
    "ipiranga", // irish Dot-Irish LLC
    "irish", // ist Istanbul Metropolitan Municipality
    "ist", // istanbul Istanbul Metropolitan Municipality / Medya A.S.
    "istanbul", // itau Itau Unibanco Holding S.A.
    "itau", // iwc Richemont DNS Inc.
    "iwc", // jaguar Jaguar Land Rover Ltd
    "jaguar", // java Oracle Corporation
    "java", // jcb JCB Co., Ltd.
    "jcb", // jetzt New TLD Company AB
    "jetzt", // jewelry Wild Bloom, LLC
    "jewelry", // jlc Richemont DNS Inc.
    "jlc", // jll Jones Lang LaSalle Incorporated
    "jll", // jobs Employ Media LLC
    "jobs", // joburg ZA Central Registry NPC trading as ZA Central Registry
    "joburg", // jprs Japan Registry Services Co., Ltd.
    "jprs", // juegos Uniregistry, Corp.
    "juegos", // kaufen United TLD Holdco Ltd.
    "kaufen", // kddi KDDI CORPORATION
    "kddi", // kia KIA MOTORS CORPORATION
    "kia", // kim Afilias Limited
    "kim", // kinder Ferrero Trading Lux S.A.
    "kinder", // kitchen Just Goodbye, LLC
    "kitchen", // kiwi DOT KIWI LIMITED
    "kiwi", // koeln NetCologne Gesellschaft für Telekommunikation mbH
    "koeln", // komatsu Komatsu Ltd.
    "komatsu", // krd KRG Department of Information Technology
    "krd", // kred KredTLD Pty Ltd
    "kred", // kyoto Academic Institution: Kyoto Jyoho Gakuen
    "kyoto", // lacaixa CAIXA D&#39;ESTALVIS I PENSIONS DE BARCELONA
    "lacaixa", // lancaster LANCASTER
    "lancaster", // land Pine Moon, LLC
    "land", // landrover Jaguar Land Rover Ltd
    "landrover", // lasalle Jones Lang LaSalle Incorporated
    "lasalle", // lat ECOM-LAC Federación de Latinoamérica y el Caribe para Internet y el Comercio Electrónico
    "lat", // latrobe La Trobe University
    "latrobe", // law Minds + Machines Group Limited
    "law", // lawyer United TLD Holdco, Ltd
    "lawyer", // lds IRI Domain Management, LLC
    "lds", // lease Victor Trail, LLC
    "lease", // leclerc A.C.D. LEC Association des Centres Distributeurs Edouard Leclerc
    "leclerc", // legal Blue Falls, LLC
    "legal", // lexus TOYOTA MOTOR CORPORATION
    "lexus", // lgbt Afilias Limited
    "lgbt", // liaison Liaison Technologies, Incorporated
    "liaison", // lidl Schwarz Domains und Services GmbH &amp; Co. KG
    "lidl", // life Trixy Oaks, LLC
    "life", // lighting John McCook, LLC
    "lighting", // limited Big Fest, LLC
    "limited", // limo Hidden Frostbite, LLC
    "limo", // linde Linde Aktiengesellschaft
    "linde", // link Uniregistry, Corp.
    "link", // live United TLD Holdco Ltd.
    "live", // lixil LIXIL Group Corporation
    "lixil", // loan dot Loan Limited
    "loan", // loans June Woods, LLC
    "loans", // lol Uniregistry, Corp.
    "lol", // london Dot London Domains Limited
    "london", // lotte Lotte Holdings Co., Ltd.
    "lotte", // lotto Afilias Limited
    "lotto", // love Merchant Law Group LLP
    "love", // ltd Over Corner, LLC
    "ltd", // ltda InterNetX Corp.
    "ltda", // lupin LUPIN LIMITED
    "lupin", // luxe Top Level Domain Holdings Limited
    "luxe", // luxury Luxury Partners LLC
    "luxury", // madrid Comunidad de Madrid
    "madrid", // maif Mutuelle Assurance Instituteur France (MAIF)
    "maif", // maison Victor Frostbite, LLC
    "maison", // man MAN SE
    "man", // management John Goodbye, LLC
    "management", // mango PUNTO FA S.L.
    "mango", // market Unitied TLD Holdco, Ltd
    "market", // marketing Fern Pass, LLC
    "marketing", // markets DOTMARKETS REGISTRY LTD
    "markets", // marriott Marriott Worldwide Corporation
    "marriott", // mba Lone Hollow, LLC
    "mba", // media Grand Glen, LLC
    "media", // meet Afilias Limited
    "meet", // melbourne The Crown in right of the State of Victoria, represented by its Department of State Development, Business and Innovation
    "melbourne", // meme Charleston Road Registry Inc.
    "meme", // memorial Dog Beach, LLC
    "memorial", // men Exclusive Registry Limited
    "men", // menu Wedding TLD2, LLC
    "menu", // meo PT Comunicacoes S.A.
    "meo", // miami Top Level Domain Holdings Limited
    "miami", // microsoft Microsoft Corporation
    "microsoft", // mil DoD Network Information Center
    "mil", // mini Bayerische Motoren Werke Aktiengesellschaft
    "mini", // mma MMA IARD
    "mma", // mobi Afilias Technologies Limited dba dotMobi
    "mobi", // moda United TLD Holdco Ltd.
    "moda", // moe Interlink Co., Ltd.
    "moe", // moi Amazon Registry Services, Inc.
    "moi", // mom Uniregistry, Corp.
    "mom", // monash Monash University
    "monash", // money Outer McCook, LLC
    "money", // montblanc Richemont DNS Inc.
    "montblanc", // mormon IRI Domain Management, LLC (&quot;Applicant&quot;)
    "mormon", // mortgage United TLD Holdco, Ltd
    "mortgage", // moscow Foundation for Assistance for Internet Technologies and Infrastructure Development (FAITID)
    "moscow", // motorcycles DERMotorcycles, LLC
    "motorcycles", // mov Charleston Road Registry Inc.
    "mov", // movie New Frostbite, LLC
    "movie", // movistar Telefónica S.A.
    "movistar", // mtn MTN Dubai Limited
    "mtn", // mtpc Mitsubishi Tanabe Pharma Corporation
    "mtpc", // mtr MTR Corporation Limited
    "mtr", // museum Museum Domain Management Association
    "museum", // mutuelle Fédération Nationale de la Mutualité Française
    "mutuelle", // nadex Nadex Domains, Inc
    "nadex", // nagoya GMO Registry, Inc.
    "nagoya", // name VeriSign Information Services, Inc.
    "name", // navy United TLD Holdco Ltd.
    "navy", // nec NEC Corporation
    "nec", // net VeriSign Global Registry Services
    "net", // netbank COMMONWEALTH BANK OF AUSTRALIA
    "netbank", // network Trixy Manor, LLC
    "network", // neustar NeuStar, Inc.
    "neustar", // new Charleston Road Registry Inc.
    "new", // news United TLD Holdco Ltd.
    "news", // nexus Charleston Road Registry Inc.
    "nexus", // ngo Public Interest Registry
    "ngo", // nhk Japan Broadcasting Corporation (NHK)
    "nhk", // nico DWANGO Co., Ltd.
    "nico", // ninja United TLD Holdco Ltd.
    "ninja", // nissan NISSAN MOTOR CO., LTD.
    "nissan", // nokia Nokia Corporation
    "nokia", // nra NRA Holdings Company, INC.
    "nra", // nrw Minds + Machines GmbH
    "nrw", // ntt NIPPON TELEGRAPH AND TELEPHONE CORPORATION
    "ntt", // nyc The City of New York by and through the New York City Department of Information Technology &amp; Telecommunications
    "nyc", // obi OBI Group Holding SE &amp; Co. KGaA
    "obi", // office Microsoft Corporation
    "office", // okinawa BusinessRalliart inc.
    "okinawa", // omega The Swatch Group Ltd
    "omega", // one One.com A/S
    "one", // ong Public Interest Registry
    "ong", // onl I-REGISTRY Ltd., Niederlassung Deutschland
    "onl", // online DotOnline Inc.
    "online", // ooo INFIBEAM INCORPORATION LIMITED
    "ooo", // oracle Oracle Corporation
    "oracle", // orange Orange Brand Services Limited
    "orange", // org Public Interest Registry (PIR)
    "org", // organic Afilias Limited
    "organic", // osaka Interlink Co., Ltd.
    "osaka", // otsuka Otsuka Holdings Co., Ltd.
    "otsuka", // ovh OVH SAS
    "ovh", // page Charleston Road Registry Inc.
    "page", // panerai Richemont DNS Inc.
    "panerai", // paris City of Paris
    "paris", // partners Magic Glen, LLC
    "partners", // parts Sea Goodbye, LLC
    "parts", // party Blue Sky Registry Limited
    "party", // pet Afilias plc
    "pet", // pharmacy National Association of Boards of Pharmacy
    "pharmacy", // philips Koninklijke Philips N.V.
    "philips", // photo Uniregistry, Corp.
    "photo", // photography Sugar Glen, LLC
    "photography", // photos Sea Corner, LLC
    "photos", // physio PhysBiz Pty Ltd
    "physio", // piaget Richemont DNS Inc.
    "piaget", // pics Uniregistry, Corp.
    "pics", // pictet Pictet Europe S.A.
    "pictet", // pictures Foggy Sky, LLC
    "pictures", // ping Ping Registry Provider, Inc.
    "ping", // pink Afilias Limited
    "pink", // pizza Foggy Moon, LLC
    "pizza", // place Snow Galley, LLC
    "place", // play Charleston Road Registry Inc.
    "play", // playstation Sony Computer Entertainment Inc.
    "playstation", // plumbing Spring Tigers, LLC
    "plumbing", // plus Sugar Mill, LLC
    "plus", // pohl Deutsche Vermögensberatung Aktiengesellschaft DVAG
    "pohl", // poker Afilias Domains No. 5 Limited
    "poker", // porn ICM Registry PN LLC
    "porn", // post Universal Postal Union
    "post", // praxi Praxi S.p.A.
    "praxi", // press DotPress Inc.
    "press", // pro Registry Services Corporation dba RegistryPro
    "pro", // prod Charleston Road Registry Inc.
    "prod", // productions Magic Birch, LLC
    "productions", // prof Charleston Road Registry Inc.
    "prof", // properties Big Pass, LLC
    "properties", // property Uniregistry, Corp.
    "property", // protection XYZ.COM LLC
    "protection", // pub United TLD Holdco Ltd.
    "pub", // qpon dotCOOL, Inc.
    "qpon", // quebec PointQuébec Inc
    "quebec", // racing Premier Registry Limited
    "racing", // realtor Real Estate Domains LLC
    "realtor", // realty Fegistry, LLC
    "realty", // recipes Grand Island, LLC
    "recipes", // red Afilias Limited
    "red", // redstone Redstone Haute Couture Co., Ltd.
    "redstone", // rehab United TLD Holdco Ltd.
    "rehab", // reise Foggy Way, LLC
    "reise", // reisen New Cypress, LLC
    "reisen", // reit National Association of Real Estate Investment Trusts, Inc.
    "reit", // ren Beijing Qianxiang Wangjing Technology Development Co., Ltd.
    "ren", // rent XYZ.COM LLC
    "rent", // rentals Big Hollow,LLC
    "rentals", // repair Lone Sunset, LLC
    "repair", // report Binky Glen, LLC
    "report", // republican United TLD Holdco Ltd.
    "republican", // rest Punto 2012 Sociedad Anonima Promotora de Inversion de Capital Variable
    "rest", // restaurant Snow Avenue, LLC
    "restaurant", // review dot Review Limited
    "review", // reviews United TLD Holdco, Ltd.
    "reviews", // rich I-REGISTRY Ltd., Niederlassung Deutschland
    "rich", // ricoh Ricoh Company, Ltd.
    "ricoh", // rio Empresa Municipal de Informática SA - IPLANRIO
    "rio", // rip United TLD Holdco Ltd.
    "rip", // rocher Ferrero Trading Lux S.A.
    "rocher", // rocks United TLD Holdco, LTD.
    "rocks", // rodeo Top Level Domain Holdings Limited
    "rodeo", // rsvp Charleston Road Registry Inc.
    "rsvp", // ruhr regiodot GmbH &amp; Co. KG
    "ruhr", // run Snow Park, LLC
    "run", // rwe RWE AG
    "rwe", // ryukyu BusinessRalliart inc.
    "ryukyu", // saarland dotSaarland GmbH
    "saarland", // sakura SAKURA Internet Inc.
    "sakura", // sale United TLD Holdco, Ltd
    "sale", // samsung SAMSUNG SDS CO., LTD
    "samsung", // sandvik Sandvik AB
    "sandvik", // sandvikcoromant Sandvik AB
    "sandvikcoromant", // sanofi Sanofi
    "sanofi", // sap SAP AG
    "sap", // sapo PT Comunicacoes S.A.
    "sapo", // sarl Delta Orchard, LLC
    "sarl", // saxo Saxo Bank A/S
    "saxo", // sbs SPECIAL BROADCASTING SERVICE CORPORATION
    "sbs", // sca SVENSKA CELLULOSA AKTIEBOLAGET SCA (publ)
    "sca", // scb The Siam Commercial Bank Public Company Limited (&quot;SCB&quot;)
    "scb", // schmidt SALM S.A.S.
    "schmidt", // scholarships Scholarships.com, LLC
    "scholarships", // school Little Galley, LLC
    "school", // schule Outer Moon, LLC
    "schule", // schwarz Schwarz Domains und Services GmbH &amp; Co. KG
    "schwarz", // science dot Science Limited
    "science", // scor SCOR SE
    "scor", // scot Dot Scot Registry Limited
    "scot", // seat SEAT, S.A. (Sociedad Unipersonal)
    "seat", // security XYZ.COM LLC
    "security", // seek Seek Limited
    "seek", // sener Sener Ingeniería y Sistemas, S.A.
    "sener", // services Fox Castle, LLC
    "services", // seven Seven West Media Ltd
    "seven", // sew SEW-EURODRIVE GmbH &amp; Co KG
    "sew", // sex ICM Registry SX LLC
    "sex", // sexy Uniregistry, Corp.
    "sexy", // shiksha Afilias Limited
    "shiksha", // shoes Binky Galley, LLC
    "shoes", // show Snow Beach, LLC
    "show", // shriram Shriram Capital Ltd.
    "shriram", // singles Fern Madison, LLC
    "singles", // site DotSite Inc.
    "site", // ski STARTING DOT LIMITED
    "ski", // sky Sky International AG
    "sky", // skype Microsoft Corporation
    "skype", // sncf SNCF (Société Nationale des Chemins de fer Francais)
    "sncf", // soccer Foggy Shadow, LLC
    "soccer", // social United TLD Holdco Ltd.
    "social", // software United TLD Holdco, Ltd
    "software", // sohu Sohu.com Limited
    "sohu", // solar Ruby Town, LLC
    "solar", // solutions Silver Cover, LLC
    "solutions", // sony Sony Corporation
    "sony", // soy Charleston Road Registry Inc.
    "soy", // space DotSpace Inc.
    "space", // spiegel SPIEGEL-Verlag Rudolf Augstein GmbH &amp; Co. KG
    "spiegel", // spreadbetting DOTSPREADBETTING REGISTRY LTD
    "spreadbetting", // srl InterNetX Corp.
    "srl", // stada STADA Arzneimittel AG
    "stada", // starhub StarHub Limited
    "starhub", // statoil Statoil ASA
    "statoil", // stc Saudi Telecom Company
    "stc", // stcgroup Saudi Telecom Company
    "stcgroup", // stockholm Stockholms kommun
    "stockholm", // studio United TLD Holdco Ltd.
    "studio", // study OPEN UNIVERSITIES AUSTRALIA PTY LTD
    "study", // style Binky Moon, LLC
    "style", // sucks Vox Populi Registry Ltd.
    "sucks", // supplies Atomic Fields, LLC
    "supplies", // supply Half Falls, LLC
    "supply", // support Grand Orchard, LLC
    "support", // surf Top Level Domain Holdings Limited
    "surf", // surgery Tin Avenue, LLC
    "surgery", // suzuki SUZUKI MOTOR CORPORATION
    "suzuki", // swatch The Swatch Group Ltd
    "swatch", // swiss Swiss Confederation
    "swiss", // sydney State of New South Wales, Department of Premier and Cabinet
    "sydney", // systems Dash Cypress, LLC
    "systems", // taipei Taipei City Government
    "taipei", // tatamotors Tata Motors Ltd
    "tatamotors", // tatar Limited Liability Company &quot;Coordination Center of Regional Domain of Tatarstan Republic&quot;
    "tatar", // tattoo Uniregistry, Corp.
    "tattoo", // tax Storm Orchard, LLC
    "tax", // taxi Pine Falls, LLC
    "taxi", // team Atomic Lake, LLC
    "team", // tech Dot Tech LLC
    "tech", // technology Auburn Falls, LLC
    "technology", // tel Telnic Ltd.
    "tel", // telefonica Telefónica S.A.
    "telefonica", // temasek Temasek Holdings (Private) Limited
    "temasek", // tennis Cotton Bloom, LLC
    "tennis", // thd Homer TLC, Inc.
    "thd", // theater Blue Tigers, LLC
    "theater", // theatre XYZ.COM LLC
    "theatre", // tickets Accent Media Limited
    "tickets", // tienda Victor Manor, LLC
    "tienda", // tips Corn Willow, LLC
    "tips", // tires Dog Edge, LLC
    "tires", // tirol punkt Tirol GmbH
    "tirol", // today Pearl Woods, LLC
    "today", // tokyo GMO Registry, Inc.
    "tokyo", // tools Pioneer North, LLC
    "tools", // top Jiangsu Bangning Science &amp; Technology Co.,Ltd.
    "top", // toray Toray Industries, Inc.
    "toray", // toshiba TOSHIBA Corporation
    "toshiba", // tours Sugar Station, LLC
    "tours", // town Koko Moon, LLC
    "town", // toyota TOYOTA MOTOR CORPORATION
    "toyota", // toys Pioneer Orchard, LLC
    "toys", // trade Elite Registry Limited
    "trade", // trading DOTTRADING REGISTRY LTD
    "trading", // training Wild Willow, LLC
    "training", // travel Tralliance Registry Management Company, LLC.
    "travel", // trust Artemis Internet Inc
    "trust", // tui TUI AG
    "tui", // ubs UBS AG
    "ubs", // university Little Station, LLC
    "university", // uno Dot Latin LLC
    "uno", // uol UBN INTERNET LTDA.
    "uol", // vacations Atomic Tigers, LLC
    "vacations", // vegas Dot Vegas, Inc.
    "vegas", // ventures Binky Lake, LLC
    "ventures", // versicherung dotversicherung-registry GmbH
    "versicherung", // vet United TLD Holdco, Ltd
    "vet", // viajes Black Madison, LLC
    "viajes", // video United TLD Holdco, Ltd
    "video", // villas New Sky, LLC
    "villas", // vin Holly Shadow, LLC
    "vin", // virgin Virgin Enterprises Limited
    "virgin", // vision Koko Station, LLC
    "vision", // vista Vistaprint Limited
    "vista", // vistaprint Vistaprint Limited
    "vistaprint", // viva Saudi Telecom Company
    "viva", // vlaanderen DNS.be vzw
    "vlaanderen", // vodka Top Level Domain Holdings Limited
    "vodka", // vote Monolith Registry LLC
    "vote", // voting Valuetainment Corp.
    "voting", // voto Monolith Registry LLC
    "voto", // voyage Ruby House, LLC
    "voyage", // wales Nominet UK
    "wales", // walter Sandvik AB
    "walter", // wang Zodiac Registry Limited
    "wang", // watch Sand Shadow, LLC
    "watch", // webcam dot Webcam Limited
    "webcam", // website DotWebsite Inc.
    "website", // wed Atgron, Inc.
    "wed", // wedding Top Level Domain Holdings Limited
    "wedding", // weir Weir Group IP Limited
    "weir", // whoswho Who&#39;s Who Registry
    "whoswho", // wien punkt.wien GmbH
    "wien", // wiki Top Level Design, LLC
    "wiki", // williamhill William Hill Organization Limited
    "williamhill", // win First Registry Limited
    "win", // windows Microsoft Corporation
    "windows", // wine June Station, LLC
    "wine", // wme William Morris Endeavor Entertainment, LLC
    "wme", // work Top Level Domain Holdings Limited
    "work", // works Little Dynamite, LLC
    "works", // world Bitter Fields, LLC
    "world", // wtc World Trade Centers Association, Inc.
    "wtc", // wtf Hidden Way, LLC
    "wtf", // xbox Microsoft Corporation
    "xbox", // xerox Xerox DNHC LLC
    "xerox", // xin Elegant Leader Limited
    "xin", // कॉम VeriSign Sarl
    "xn--11b4c3d", // 佛山 Guangzhou YU Wei Information Technology Co., Ltd.
    "xn--1qqw23a", // 慈善 Excellent First Limited
    "xn--30rr7y", // 集团 Eagle Horizon Limited
    "xn--3bst00m", // 在线 TLD REGISTRY LIMITED
    "xn--3ds443g", // 点看 VeriSign Sarl
    "xn--3pxu8k", // คอม VeriSign Sarl
    "xn--42c2d9a", // 八卦 Zodiac Scorpio Limited
    "xn--45q11c", // موقع Suhub Electronic Establishment
    "xn--4gbrim", // 公益 China Organizational Name Administration Center
    "xn--55qw42g", // 公司 Computer Network Information Center of Chinese Academy of Sciences （China Internet Network Information Center）
    "xn--55qx5d", // 移动 Afilias Limited
    "xn--6frz82g", // 我爱你 Tycoon Treasure Limited
    "xn--6qq986b3xl", // москва Foundation for Assistance for Internet Technologies and Infrastructure Development (FAITID)
    "xn--80adxhks", // онлайн CORE Association
    "xn--80asehdb", // сайт CORE Association
    "xn--80aswg", // קום VeriSign Sarl
    "xn--9dbq2a", // 时尚 RISE VICTORY LIMITED
    "xn--9et52u", // 淡马锡 Temasek Holdings (Private) Limited
    "xn--b4w605ferd", // орг Public Interest Registry
    "xn--c1avg", // नेट VeriSign Sarl
    "xn--c2br7g", // 삼성 SAMSUNG SDS CO., LTD
    "xn--cg4bki", // 商标 HU YI GLOBAL INFORMATION RESOURCES(HOLDING) COMPANY.HONGKONG LIMITED
    "xn--czr694b", // 商店 Wild Island, LLC
    "xn--czrs0t", // 商城 Zodiac Aquarius Limited
    "xn--czru2d", // дети The Foundation for Network Initiatives “The Smart Internet”
    "xn--d1acj3b", // 新闻 Xinhua News Agency Guangdong Branch 新华通讯社广东分社
    "xn--efvy88h", // 工行 Industrial and Commercial Bank of China Limited
    "xn--estv75g", // كوم VeriSign Sarl
    "xn--fhbei", // 中文网 TLD REGISTRY LIMITED
    "xn--fiq228c5hs", // 中信 CITIC Group Corporation
    "xn--fiq64b", // 娱乐 Will Bloom, LLC
    "xn--fjq720a", // 谷歌 Charleston Road Registry Inc.
    "xn--flw351e", // 网店 Zodiac Libra Limited
    "xn--hxt814e", // संगठन Public Interest Registry
    "xn--i1b6b1a6a2e", // 餐厅 HU YI GLOBAL INFORMATION RESOURCES (HOLDING) COMPANY. HONGKONG LIMITED
    "xn--imr513n", // 网络 Computer Network Information Center of Chinese Academy of Sciences （China Internet Network Information Center）
    "xn--io0a7i", // ком VeriSign Sarl
    "xn--j1aef", // 飞利浦 Koninklijke Philips N.V.
    "xn--kcrx77d1x4a", // 手机 Beijing RITT-Net Technology Development Co., Ltd
    "xn--kput3i", // ارامكو Aramco Services Company
    "xn--mgba3a3ejt", // بازار CORE Association
    "xn--mgbab2bd", // 닷컴 VeriSign Sarl
    "xn--mk1bu44c", // 政府 Net-Chinese Co., Ltd.
    "xn--mxtq1m", // شبكة International Domain Registry Pty. Ltd.
    "xn--ngbc5azd", // 机构 Public Interest Registry
    "xn--nqv7f", // 组织机构 Public Interest Registry
    "xn--nqv7fs00ema", // 健康 Stable Tone Limited
    "xn--nyqy26a", // рус Rusnames Limited
    "xn--p1acf", // 大拿 VeriSign Sarl
    "xn--pssy2u", // みんな Charleston Road Registry Inc.
    "xn--q9jyb4c", // グーグル Charleston Road Registry Inc.
    "xn--qcka1pmc", // 世界 Stable Tone Limited
    "xn--rhqv96g", // 网址 KNET Co., Ltd
    "xn--ses554g", // 닷넷 VeriSign Sarl
    "xn--t60b56a", // コム VeriSign Sarl
    "xn--tckwe", // 游戏 Spring Fields, LLC
    "xn--unup4y", // VERMöGENSBERATER Deutsche Vermögensberatung Aktiengesellschaft DVAG
    "xn--vermgensberater-ctb", // VERMöGENSBERATUNG Deutsche Vermögensberatung Aktiengesellschaft DVAG
    "xn--vermgensberatung-pwb", // 企业 Dash McCook, LLC
    "xn--vhquv", // 信息 Beijing Tele-info Network Technology Co., Ltd.
    "xn--vuq861b", // 广东 Guangzhou YU Wei Information Technology Co., Ltd.
    "xn--xhq521b", // 政务 China Organizational Name Administration Center
    "xn--zfr164b", // xperia Sony Mobile Communications AB
    "xperia", // xxx ICM Registry LLC
    "xxx", // xyz XYZ.COM LLC
    "xyz", // yachts DERYachts, LLC
    "yachts", // yamaxun Amazon Registry Services, Inc.
    "yamaxun", // yandex YANDEX, LLC
    "yandex", // yodobashi YODOBASHI CAMERA CO.,LTD.
    "yodobashi", // yoga Top Level Domain Holdings Limited
    "yoga", // yokohama GMO Registry, Inc.
    "yokohama", // youtube Charleston Road Registry Inc.
    "youtube", // zara Industria de Diseño Textil, S.A. (INDITEX, S.A.)
    "zara", // zip Charleston Road Registry Inc.
    "zip", // zone Outer Falls, LLC
    "zone", // zuerich Kanton Zürich (Canton of Zurich)
    "zuerich" };

    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static final String[] COUNTRY_CODE_TLDS = new String[] { // Ascension Island
    "ac", // Andorra
    "ad", // United Arab Emirates
    "ae", // Afghanistan
    "af", // Antigua and Barbuda
    "ag", // Anguilla
    "ai", // Albania
    "al", // Armenia
    "am", // Angola
    "ao", // Antarctica
    "aq", // Argentina
    "ar", // American Samoa
    "as", // Austria
    "at", // Australia (includes Ashmore and Cartier Islands and Coral Sea Islands)
    "au", // Aruba
    "aw", // Åland
    "ax", // Azerbaijan
    "az", // Bosnia and Herzegovina
    "ba", // Barbados
    "bb", // Bangladesh
    "bd", // Belgium
    "be", // Burkina Faso
    "bf", // Bulgaria
    "bg", // Bahrain
    "bh", // Burundi
    "bi", // Benin
    "bj", // Bermuda
    "bm", // Brunei Darussalam
    "bn", // Bolivia
    "bo", // Brazil
    "br", // Bahamas
    "bs", // Bhutan
    "bt", // Bouvet Island
    "bv", // Botswana
    "bw", // Belarus
    "by", // Belize
    "bz", // Canada
    "ca", // Cocos (Keeling) Islands
    "cc", // Democratic Republic of the Congo (formerly Zaire)
    "cd", // Central African Republic
    "cf", // Republic of the Congo
    "cg", // Switzerland
    "ch", // Côte d'Ivoire
    "ci", // Cook Islands
    "ck", // Chile
    "cl", // Cameroon
    "cm", // China, mainland
    "cn", // Colombia
    "co", // Costa Rica
    "cr", // Cuba
    "cu", // Cape Verde
    "cv", // Curaçao
    "cw", // Christmas Island
    "cx", // Cyprus
    "cy", // Czech Republic
    "cz", // Germany
    "de", // Djibouti
    "dj", // Denmark
    "dk", // Dominica
    "dm", // Dominican Republic
    "do", // Algeria
    "dz", // Ecuador
    "ec", // Estonia
    "ee", // Egypt
    "eg", // Eritrea
    "er", // Spain
    "es", // Ethiopia
    "et", // European Union
    "eu", // Finland
    "fi", // Fiji
    "fj", // Falkland Islands
    "fk", // Federated States of Micronesia
    "fm", // Faroe Islands
    "fo", // France
    "fr", // Gabon
    "ga", // Great Britain (United Kingdom)
    "gb", // Grenada
    "gd", // Georgia
    "ge", // French Guiana
    "gf", // Guernsey
    "gg", // Ghana
    "gh", // Gibraltar
    "gi", // Greenland
    "gl", // The Gambia
    "gm", // Guinea
    "gn", // Guadeloupe
    "gp", // Equatorial Guinea
    "gq", // Greece
    "gr", // South Georgia and the South Sandwich Islands
    "gs", // Guatemala
    "gt", // Guam
    "gu", // Guinea-Bissau
    "gw", // Guyana
    "gy", // Hong Kong
    "hk", // Heard Island and McDonald Islands
    "hm", // Honduras
    "hn", // Croatia (Hrvatska)
    "hr", // Haiti
    "ht", // Hungary
    "hu", // Indonesia
    "id", // Ireland (Éire)
    "ie", // Israel
    "il", // Isle of Man
    "im", // India
    "in", // British Indian Ocean Territory
    "io", // Iraq
    "iq", // Iran
    "ir", // Iceland
    "is", // Italy
    "it", // Jersey
    "je", // Jamaica
    "jm", // Jordan
    "jo", // Japan
    "jp", // Kenya
    "ke", // Kyrgyzstan
    "kg", // Cambodia (Khmer)
    "kh", // Kiribati
    "ki", // Comoros
    "km", // Saint Kitts and Nevis
    "kn", // North Korea
    "kp", // South Korea
    "kr", // Kuwait
    "kw", // Cayman Islands
    "ky", // Kazakhstan
    "kz", // Laos (currently being marketed as the official domain for Los Angeles)
    "la", // Lebanon
    "lb", // Saint Lucia
    "lc", // Liechtenstein
    "li", // Sri Lanka
    "lk", // Liberia
    "lr", // Lesotho
    "ls", // Lithuania
    "lt", // Luxembourg
    "lu", // Latvia
    "lv", // Libya
    "ly", // Morocco
    "ma", // Monaco
    "mc", // Moldova
    "md", // Montenegro
    "me", // Madagascar
    "mg", // Marshall Islands
    "mh", // Republic of Macedonia
    "mk", // Mali
    "ml", // Myanmar
    "mm", // Mongolia
    "mn", // Macau
    "mo", // Northern Mariana Islands
    "mp", // Martinique
    "mq", // Mauritania
    "mr", // Montserrat
    "ms", // Malta
    "mt", // Mauritius
    "mu", // Maldives
    "mv", // Malawi
    "mw", // Mexico
    "mx", // Malaysia
    "my", // Mozambique
    "mz", // Namibia
    "na", // New Caledonia
    "nc", // Niger
    "ne", // Norfolk Island
    "nf", // Nigeria
    "ng", // Nicaragua
    "ni", // Netherlands
    "nl", // Norway
    "no", // Nepal
    "np", // Nauru
    "nr", // Niue
    "nu", // New Zealand
    "nz", // Oman
    "om", // Panama
    "pa", // Peru
    "pe", // French Polynesia With Clipperton Island
    "pf", // Papua New Guinea
    "pg", // Philippines
    "ph", // Pakistan
    "pk", // Poland
    "pl", // Saint-Pierre and Miquelon
    "pm", // Pitcairn Islands
    "pn", // Puerto Rico
    "pr", // Palestinian territories (PA-controlled West Bank and Gaza Strip)
    "ps", // Portugal
    "pt", // Palau
    "pw", // Paraguay
    "py", // Qatar
    "qa", // Réunion
    "re", // Romania
    "ro", // Serbia
    "rs", // Russia
    "ru", // Rwanda
    "rw", // Saudi Arabia
    "sa", // Solomon Islands
    "sb", // Seychelles
    "sc", // Sudan
    "sd", // Sweden
    "se", // Singapore
    "sg", // Saint Helena
    "sh", // Slovenia
    "si", // Svalbard and Jan Mayen Islands Not in use (Norwegian dependencies; see .no)
    "sj", // Slovakia
    "sk", // Sierra Leone
    "sl", // San Marino
    "sm", // Senegal
    "sn", // Somalia
    "so", // Suriname
    "sr", // São Tomé and Príncipe
    "st", // Soviet Union (deprecated)
    "su", // El Salvador
    "sv", // Sint Maarten
    "sx", // Syria
    "sy", // Swaziland
    "sz", // Turks and Caicos Islands
    "tc", // Chad
    "td", // French Southern and Antarctic Lands
    "tf", // Togo
    "tg", // Thailand
    "th", // Tajikistan
    "tj", // Tokelau
    "tk", // East Timor (deprecated old code)
    "tl", // Turkmenistan
    "tm", // Tunisia
    "tn", // Tonga
    "to", // Turkey
    "tr", // Trinidad and Tobago
    "tt", // Tuvalu
    "tv", // Taiwan, Republic of China
    "tw", // Tanzania
    "tz", // Ukraine
    "ua", // Uganda
    "ug", // United Kingdom
    "uk", // United States of America
    "us", // Uruguay
    "uy", // Uzbekistan
    "uz", // Vatican City State
    "va", // Saint Vincent and the Grenadines
    "vc", // Venezuela
    "ve", // British Virgin Islands
    "vg", // U.S. Virgin Islands
    "vi", // Vietnam
    "vn", // Vanuatu
    "vu", // Wallis and Futuna
    "wf", // Samoa (formerly Western Samoa)
    "ws", // 한국 KISA (Korea Internet &amp; Security Agency)
    "xn--3e0b707e", // ভারত National Internet Exchange of India
    "xn--45brj9c", // қаз Association of IT Companies of Kazakhstan
    "xn--80ao21a", // срб Serbian National Internet Domain Registry (RNIDS)
    "xn--90a3ac", // ??? Reliable Software Inc.
    "xn--90ais", // சிங்கப்பூர் Singapore Network Information Centre (SGNIC) Pte Ltd
    "xn--clchc0ea0b2g2a9gcd", // мкд Macedonian Academic Research Network Skopje
    "xn--d1alf", // 中国 China Internet Network Information Center
    "xn--fiqs8s", // 中國 China Internet Network Information Center
    "xn--fiqz9s", // భారత్ National Internet Exchange of India
    "xn--fpcrj9c3d", // ලංකා LK Domain Registry
    "xn--fzc2c9e2c", // ભારત National Internet Exchange of India
    "xn--gecrj9c", // भारत National Internet Exchange of India
    "xn--h2brj9c", // укр Ukrainian Network Information Centre (UANIC), Inc.
    "xn--j1amh", // 香港 Hong Kong Internet Registration Corporation Ltd.
    "xn--j6w193g", // 台湾 Taiwan Network Information Center (TWNIC)
    "xn--kprw13d", // 台灣 Taiwan Network Information Center (TWNIC)
    "xn--kpry57d", // мон Datacom Co.,Ltd
    "xn--l1acc", // الجزائر CERIST
    "xn--lgbbat1ad8j", // عمان Telecommunications Regulatory Authority (TRA)
    "xn--mgb9awbf", // ایران Institute for Research in Fundamental Sciences (IPM)
    "xn--mgba3a4f16a", // امارات Telecommunications Regulatory Authority (TRA)
    "xn--mgbaam7a8h", // الاردن National Information Technology Center (NITC)
    "xn--mgbayh7gpa", // بھارت National Internet Exchange of India
    "xn--mgbbh1a71e", // المغرب Agence Nationale de Réglementation des Télécommunications (ANRT)
    "xn--mgbc0a9azcg", // السعودية Communications and Information Technology Commission
    "xn--mgberp4a5d4ar", // ????? Sudan Internet Society
    "xn--mgbpl2fh", // عراق Communications and Media Commission (CMC)
    "xn--mgbtx2b", // مليسيا MYNIC Berhad
    "xn--mgbx4cd0ab", // გე Information Technologies Development Center (ITDC)
    "xn--node", // ไทย Thai Network Information Center Foundation
    "xn--o3cw4h", // سورية National Agency for Network Services (NANS)
    "xn--ogbpf8fl", // рф Coordination Center for TLD RU
    "xn--p1ai", // تونس Agence Tunisienne d&#39;Internet
    "xn--pgbs0dh", // ελ ICS-FORTH GR
    "xn--qxam", // ਭਾਰਤ National Internet Exchange of India
    "xn--s9brj9c", // مصر National Telecommunication Regulatory Authority - NTRA
    "xn--wgbh1c", // قطر Communications Regulatory Authority
    "xn--wgbl6a", // இலங்கை LK Domain Registry
    "xn--xkc2al3hye2a", // இந்தியா National Internet Exchange of India
    "xn--xkc2dl3a5ee0h", // ??? Internet Society
    "xn--y9a3aq", // 新加坡 Singapore Network Information Centre (SGNIC) Pte Ltd
    "xn--yfro4i67o", // فلسطين Ministry of Telecom &amp; Information Technology (MTIT)
    "xn--ygbi2ammx", // Yemen
    "ye", // Mayotte
    "yt", // South Africa
    "za", // Zambia
    "zm", // Zimbabwe
    "zw" };

    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static final String[] LOCAL_TLDS = new String[] { // Also widely used as localhost.localdomain
    "localdomain", // RFC2606 defined
    "localhost" };

    // Additional arrays to supplement or override the built in ones.
    // The PLUS arrays are valid keys, the MINUS arrays are invalid keys
    /*
     * This field is used to detect whether the getInstance has been called.
     * After this, the method updateTLDOverride is not allowed to be called.
     * This field does not need to be volatile since it is only accessed from
     * synchronized methods. 
     */
    private static boolean inUse = false;

    /*
     * These arrays are mutable, but they don't need to be volatile.
     * They can only be updated by the updateTLDOverride method, and any readers must get an instance
     * using the getInstance methods which are all (now) synchronised.
     */
    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static volatile String[] COUNTRY_CODE_TLDS_PLUS = EMPTY_STRING_ARRAY;

    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static volatile String[] GENERIC_TLDS_PLUS = EMPTY_STRING_ARRAY;

    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static volatile String[] COUNTRY_CODE_TLDS_MINUS = EMPTY_STRING_ARRAY;

    // WARNING: this array MUST be sorted, otherwise it cannot be searched reliably using binary search
    private static volatile String[] GENERIC_TLDS_MINUS = EMPTY_STRING_ARRAY;

    /**
     * enum used by {@link DomainValidator#updateTLDOverride(ArrayType, String[])}
     * to determine which override array to update.
     * @since 1.5.0
     */
    enum ArrayType {

        /**
         * Update the GENERIC_TLDS_PLUS table containing additonal generic TLDs
         */
        GENERIC_PLUS, /**
         * Update the GENERIC_TLDS_MINUS table containing deleted generic TLDs
         */
        GENERIC_MINUS, /**
         * Update the COUNTRY_CODE_TLDS_PLUS table containing additonal country code TLDs
         */
        COUNTRY_CODE_PLUS, /**
         * Update the COUNTRY_CODE_TLDS_MINUS table containing deleted country code TLDs
         */
        COUNTRY_CODE_MINUS
    }

    // For use by unit test code only
    static synchronized void clearTLDOverrides() {
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "71fb325f-b073-428c-86d8-d0aac95f3c72");
        inUse = false;
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "27b743a9-4920-4f26-8b04-fb84c3a34ebe");
        COUNTRY_CODE_TLDS_PLUS = EMPTY_STRING_ARRAY;
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "9446dc1d-bb28-457e-9907-bfadce0eec42");
        COUNTRY_CODE_TLDS_MINUS = EMPTY_STRING_ARRAY;
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "7ed7f711-d56b-4967-8bfe-10efffcde951");
        GENERIC_TLDS_PLUS = EMPTY_STRING_ARRAY;
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "61601bdb-cc0a-4c5c-94c7-05dd795444c2");
        GENERIC_TLDS_MINUS = EMPTY_STRING_ARRAY;
    }

    /**
     * Update one of the TLD override arrays.
     * This must only be done at program startup, before any instances are accessed using getInstance.
     * <p>
     * For example:
     * <p>
     * {@code DomainValidator.updateTLDOverride(ArrayType.GENERIC_PLUS, new String[]{"apache"})}
     * <p>
     * To clear an override array, provide an empty array.
     *
     * @param table the table to update, see {@link DomainValidator#ArrayType}
     * @param tlds the array of TLDs, must not be null
     * @throws IllegalStateException if the method is called after getInstance
     * @since 1.5.0
     */
    public static synchronized void updateTLDOverride(ArrayType table, String[] tlds) {
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "97a8bbd4-01ac-4756-974f-cd7a403b9a2c");
        if (inUse) {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "41516716-8d66-488b-931f-c8a43fde7963");
            throw new IllegalStateException("Can only invoke this method before calling getInstance");
        }
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "c651f0f3-546a-46c4-9619-5db9fc3be77e");
        String[] copy = new String[tlds.length];
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "3d8e4505-3203-43ad-8ff1-c3ea9dd1bcce");
        // Comparisons are always done with lower-case entries
        for (int i = 0; i < tlds.length; i++) {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "ecb1510e-0715-46cb-b9cb-4650ba2f4cd1");
            copy[i] = tlds[i].toLowerCase(Locale.ENGLISH);
        }
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "fbd9c9b4-9c94-4763-838c-7f4d33eeda4f");
        Arrays.sort(copy);
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "1655ae4f-f1cc-4b66-9a92-39880580becb");
        switch(table) {
            case COUNTRY_CODE_MINUS:
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "4a609188-7544-42fb-b4f2-afd121995439");
                COUNTRY_CODE_TLDS_MINUS = copy;
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "a3cb39c7-f025-400f-b923-82af05d0a601");
                break;
            case COUNTRY_CODE_PLUS:
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "efbe717f-2533-4e3c-97c0-4e177d5d5b93");
                COUNTRY_CODE_TLDS_PLUS = copy;
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "3c047fff-ee87-41f4-9c88-0b241f55211c");
                break;
            case GENERIC_MINUS:
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "c1091b41-e9c5-4e08-8968-f930c92628f2");
                GENERIC_TLDS_MINUS = copy;
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "9c390567-a423-456f-8c01-1169eceb1b50");
                break;
            case GENERIC_PLUS:
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "28ac98bb-9712-41d0-9459-5b280caa7c0a");
                GENERIC_TLDS_PLUS = copy;
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "512fad38-9489-447b-9f4d-ba192dda41fd");
                break;
        }
    }

    /**
     * Converts potentially Unicode input to punycode.
     * If conversion fails, returns the original input.
     *
     * @param input the string to convert, not null
     * @return converted input, or original input if conversion fails
     */
    // Needed by UrlValidator
    static String unicodeToASCII(String input) {
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "34eeed3a-64c8-467a-9ac2-c9722fdda98f");
        String returnValue = "";
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "8b30f676-ee70-442b-b622-6b670935f376");
        if (isOnlyASCII(input)) {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "42ede64a-b6ec-47ed-aa3c-e1dcbcc843a3");
            // skip possibly expensive processing
            return input;
        }
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "f529eb15-ee6e-4582-a804-5807ee619369");
        try {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "454549cc-ba75-4919-abcd-2ad1e8c2ffe6");
            final String ascii = IDN.toASCII(input);
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "a5e9ed98-15c2-4bff-9cb5-33b5c12e066f");
            if (IDNBUGHOLDER.IDN_TOASCII_PRESERVES_TRAILING_DOTS) {
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "8f14f75c-0f47-4a81-ae7c-07b3329f9752");
                return ascii;
            }
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "547a3045-9f45-42ce-b09e-dbdf98ad2565");
            final int length = input.length();
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "b1d8dd76-7f99-4477-8d35-c08bfa4fcce9");
            if (length == 0) {
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "a7ffe610-1df1-4ce5-95f8-3801b7193098");
                // check there is a last character
                return input;
            }
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "c6cbe9bd-3df5-4471-b624-dd018fc40aa5");
            // RFC3490 3.1. 1)
            // Whenever dots are used as label separators, the following
            // characters MUST be recognized as dots: U+002E (full stop), U+3002
            // (ideographic full stop), U+FF0E (fullwidth full stop), U+FF61
            // (halfwidth ideographic full stop).
            // fetch original last char
            char lastChar = input.charAt(length - 1);
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "7ce5577a-4c3a-4f91-8a88-d421cfa855f5");
            switch(lastChar) {
                case '\u002E':
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "5174c9b3-22e0-4d8f-8fb5-97462a1151c6");
                    returnValue = ascii + ".";
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "e7cd3b53-9c13-4c16-825d-15ef16bdb5ec");
                    // "." full stop
                    break;
                case '\u3002':
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "e0acbe43-b9ab-4396-bd74-1fec96f098eb");
                    returnValue = ascii + ".";
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "876916ee-0147-4390-a14b-da35dffab9b7");
                    // ideographic full stop
                    break;
                case '\uFF0E':
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "bb1fa02e-cb33-4eec-98c1-a8e82c686698");
                    returnValue = ascii + ".";
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "cb152fad-5a3c-42c2-8b74-3d8cc4e027eb");
                    // fullwidth full stop
                    break;
                case // halfwidth ideographic full stop
                '\uFF61':
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "7aff65b5-29f0-4047-91a9-75c28aab53cd");
                    returnValue = ascii + ".";
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "65c344fe-c710-40dc-89aa-5199293bda33");
                    break;
                // restore the missing stop
                default:
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "d7841823-1e50-4931-894e-966363ae5f04");
                    returnValue = ascii;
                    writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "331e4d69-929a-446d-a57a-478f88a9a04e");
                    break;
            }
        } catch (IllegalArgumentException e) {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "86d5442b-a40c-4819-a4f6-d7891a803f6b");
            // input is not valid
            return input;
        }
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "a71a3bb6-0cc7-4f85-934d-3eb9914c2af7");
        return returnValue;
    }

    private static class IDNBUGHOLDER {

        private static boolean keepsTrailingDot() {
            // must be a valid name
            final String input = "a.";
            return input.equals(IDN.toASCII(input));
        }

        private static final boolean IDN_TOASCII_PRESERVES_TRAILING_DOTS = keepsTrailingDot();
    }

    /*
     * Check if input contains only ASCII
     * Treats null as all ASCII
     */
    private static boolean isOnlyASCII(String input) {
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "61728dd0-a870-476e-b6d7-2f3f72f10dc9");
        if (input == null) {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "9cae1ac6-47d1-419b-bc14-7fc79b3cf2ff");
            return true;
        }
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "2d46ab7c-65bd-489d-abe7-d84b0e65db11");
        for (int i = 0; i < input.length(); i++) {
            writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "38c1a8a3-1faa-48ae-abed-5c431a810111");
            if (input.charAt(i) > 0x7F) {
                writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "a3d79b0e-63fa-450b-b70b-bc13199964e2");
                return false;
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "15b3943f-fd5b-47a5-a070-d639cf6cd77e");
        return true;
    }

    /**
     * Check if a sorted array contains the specified key
     *
     * @param sortedArray the array to search
     * @param key the key to find
     * @return {@code true} if the array contains the key
     */
    private static boolean arrayContains(String[] sortedArray, String key) {
        writelineStatic("/home/ubuntu/results/coverage/DomainValidator/DomainValidator_5_10.coverage", "7d4d69f8-26cb-4d60-b9a0-b2e11174efc0");
        return Arrays.binarySearch(sortedArray, key) >= 0;
    }

    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
