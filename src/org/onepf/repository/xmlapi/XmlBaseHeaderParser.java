package org.onepf.repository.xmlapi;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * Created by ivanoff on 02.04.14.
 */
public abstract class XmlBaseHeaderParser extends XmlItemParser<BaseListHeaderDescriptor> implements XMLElements.BaseHeader {


    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if (getElementName().equals(qName)) {
            descriptor = new BaseListHeaderDescriptor();
            descriptor.version = attributes.getValue(FIELD_PACKAGE);
            descriptor.lastUpdated = attributes.getValue(FIELD_LAST_UPDATED);
            descriptor.offset = attributes.getValue(FIELD_OFFSET);
        }
    }

}
