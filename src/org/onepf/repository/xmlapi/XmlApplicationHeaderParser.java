package org.onepf.repository.xmlapi;

/**
 * Created by ivanoff on 02.04.14.
 */
public class XmlApplicationHeaderParser extends XmlBaseHeaderParser implements XMLElements.ApplicationListHeader{

    @Override
    public String getElementName() {
        return ELEMENT_NAME;
    }
}
