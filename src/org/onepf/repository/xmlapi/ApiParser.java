package org.onepf.repository.xmlapi;

import org.onepf.repository.utils.Pair;
import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.BaseListHeaderDescriptor;
import org.xml.sax.SAXException;

import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.IOException;
import java.io.InputStream;
import java.security.DigestInputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collection;

/**
 * Created by ivanoff on 02.04.14.
 */
public class ApiParser {

    public static class ParseException extends Exception {

        public ParseException(Exception e) {
            super(e);
        }
    }

    private static SAXParserFactory factory = SAXParserFactory.newInstance();
    private static HexBinaryAdapter marshaler = new HexBinaryAdapter();


    private static String parse(XmlListParser parser, InputStream is) throws ParseException {
        String hash = null;
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            DigestInputStream dis = new DigestInputStream(is, md);

            SAXParser saxParser = factory.newSAXParser();
            saxParser.parse(dis, parser);
            hash = marshaler.marshal(dis.getMessageDigest().digest());
        } catch (SAXException e) {
            throw new ParseException(e);
        } catch (IOException e) {
            throw new ParseException(e);
        } catch (ParserConfigurationException e) {
            throw new ParseException(e);
        } catch (NoSuchAlgorithmException e) {
            throw new ParseException(e);
        }
        return hash;
    }

    public static Pair<BaseListHeaderDescriptor, String> getApplications(Collection<ApplicationDescriptor> apps, InputStream is) throws ParseException {
        XmlApplicationsListParser appParser = new XmlApplicationsListParser(apps);
        String hash = parse(appParser, is);
        return new Pair<BaseListHeaderDescriptor, String>(appParser.getHeader(), hash);
    }


}
