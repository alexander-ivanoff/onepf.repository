package org.onepf.repository.api.xmlapi;

import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;

/**
 * Created by ivanoff on 17.04.14.
 */
public interface JaxbElementMaker {

    <T extends JaxbElementMaker> JAXBElement<T> getAsJaxbElement(QName qname);
}
