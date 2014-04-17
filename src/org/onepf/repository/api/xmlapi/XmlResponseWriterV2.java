package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.responsewriter.ResponseWriterV2;
import org.onepf.repository.api.responsewriter.WriteException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.namespace.QName;
import java.io.OutputStream;

/**
 * Created by ivanoff on 17.04.14.
 */
public class XmlResponseWriterV2<T extends JaxbElementMaker> implements ResponseWriterV2<T>{

    private QName qName;
    private JAXBContext context;

    public XmlResponseWriterV2(QName qName, Class... classes) throws WriteException {
        try {
            context = JAXBContext.newInstance(classes);
        } catch (JAXBException e) {
            throw new WriteException(e);
        }
        this.qName = qName;
    }

    public XmlResponseWriterV2(QName qName, String packageName) throws WriteException {
        try {
            context = JAXBContext.newInstance(packageName);
        } catch (JAXBException e) {
            context = null;
        }
        this.qName = qName;
    }


    @Override
    public void write(OutputStream os, T objectToWrite) throws WriteException {
        JAXBElement<T> toWrite = objectToWrite.getAsJaxbElement(qName);
        Marshaller m = null;
        try {
            m = context.createMarshaller();
            m.marshal(toWrite, os);
        } catch (JAXBException e) {
            throw new WriteException(e);
        }
    }
}
