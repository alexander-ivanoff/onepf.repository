package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.ListParser;
import org.onepf.repository.api.ParserFactory;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationListHeaderDescriptor;
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
 * Realization of ParserFactory for xml api.
 *
 * @author Alexander Ivanoff
 */
public class XmlParserFactory extends ParserFactory<XmlListParser> {

    private static SAXParserFactory factory = SAXParserFactory.newInstance();
    private static HexBinaryAdapter marshaler = new HexBinaryAdapter();



    @Override
    public String parse(XmlListParser parser, InputStream is) throws ParseException {
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

    @Override
    public ListParser<ApplicationDescriptor, ApplicationListHeaderDescriptor> getApplicationParser(Collection<ApplicationDescriptor> apps) {
        return new XmlApplicationsListParser(apps);
    }
}
