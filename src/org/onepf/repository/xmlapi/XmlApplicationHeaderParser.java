package org.onepf.repository.xmlapi;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationListHeaderDescriptor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * Created by ivanoff on 02.04.14.
 */
public class XmlApplicationHeaderParser extends XmlItemParser<ApplicationListHeaderDescriptor> implements XMLElements.ApplicationListHeader{

    @Override
    public String getElementName() {
        return ELEMENT_NAME;
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        descriptor = new ApplicationListHeaderDescriptor(attributes.getValue(FIELD_VERSION), attributes.getValue(FIELD_OFFSET));

    }
}
