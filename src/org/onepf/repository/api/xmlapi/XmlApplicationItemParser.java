package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 *
 * XmlItemParser realization to parse applications object
 *
 * @author Alexander Ivanoff on 02.04.14.
 */
public class XmlApplicationItemParser extends XmlItemParser<ApplicationDescriptor> implements  XMLElements.Application{

    @Override
    public String getElementName() {
        return ELEMENT_NAME;
    }


    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if (ELEMENT_NAME.equals(qName)) {
            descriptor = new ApplicationDescriptor();
            descriptor.packageName = attributes.getValue(FIELD_PACKAGE);
            descriptor.appdfHash = attributes.getValue(FIELD_HASH);
        }
    }
}
