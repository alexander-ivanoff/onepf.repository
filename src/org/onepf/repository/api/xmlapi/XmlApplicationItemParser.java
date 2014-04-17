package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

/**
 * XmlItemParser realization to parse applications object
 *
 * @author Alexander Ivanoff on 02.04.14.
 */
public class XmlApplicationItemParser extends XmlItemParser<ApplicationEntity> implements XMLElements.Application {

    @Override
    public String getElementName() {
        return ELEMENT_NAME;
    }


    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if (ELEMENT_NAME.equals(qName)) {
            descriptor = new ApplicationEntity();
            descriptor.setPackageName(attributes.getValue(FIELD_PACKAGE));
            descriptor.setAppdfHash(attributes.getValue(FIELD_HASH));
        }
    }
}
