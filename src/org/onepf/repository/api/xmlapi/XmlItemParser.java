package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.ItemParser;
import org.onepf.repository.api.responsewriter.entity.BaseEntity;
import org.xml.sax.helpers.DefaultHandler;

/**
 * realization of ItemParser for xml api.
 *
 * @author Alexander Ivanoff on 02.04.14.
 */
public abstract class XmlItemParser<E extends BaseEntity> extends DefaultHandler implements ItemParser<E>{

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
