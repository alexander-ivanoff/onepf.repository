package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.ItemParser;
import org.onepf.repository.api.responsewriter.descriptors.AbstractDescriptor;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Created by ivanoff on 02.04.14.
 */
public abstract class XmlItemParser<E extends AbstractDescriptor> extends DefaultHandler implements ItemParser<E>{

    protected E descriptor;


    public E getDescriptor() {
        return descriptor;
    }

    public void cleanDesciptor() {
        descriptor = null;
    }

    public E removeDescriptor() {
        E descr = descriptor;
        descriptor = null;
        return descr;
    }

    public abstract String getElementName();
}
