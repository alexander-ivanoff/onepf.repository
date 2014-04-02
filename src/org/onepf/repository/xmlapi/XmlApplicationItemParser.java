package org.onepf.repository.xmlapi;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * Created by ivanoff on 02.04.14.
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
            descriptor.lastUpdated = attributes.getValue(FIELD_LAST_UPDATED);
        }
    }
}
