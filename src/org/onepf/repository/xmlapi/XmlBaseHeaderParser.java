package org.onepf.repository.xmlapi;

import org.onepf.repository.utils.responsewriter.descriptors.BaseListHeaderDescriptor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * Created by ivanoff on 02.04.14.
 */
public abstract class XmlBaseHeaderParser extends XmlItemParser<BaseListHeaderDescriptor> implements XMLElements.BaseHeader {


    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if (getElementName().equals(qName)) {
            descriptor = new BaseListHeaderDescriptor(attributes.getValue(FIELD_VERSION), attributes.getValue(FIELD_OFFSET));
        }
    }

}
