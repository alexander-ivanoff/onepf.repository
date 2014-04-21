package org.onepf.repository.api.xmlapi;

import org.onepf.repository.api.responsewriter.ResponseReaderWriter;
import org.onepf.repository.api.responsewriter.WriteException;

import javax.xml.bind.*;
import javax.xml.namespace.QName;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Created by ivanoff on 17.04.14.
 */
public class XmlResponseReaderWriter<T extends JaxbElementMaker> implements ResponseReaderWriter<T> {

    private QName qName;
    private JAXBContext context;

    public XmlResponseReaderWriter(QName qName, Class... classes) throws WriteException {
        try {
            context = JAXBContext.newInstance(classes);
        } catch (JAXBException e) {
            throw new WriteException(e);
        }
        this.qName = qName;
    }

    public XmlResponseReaderWriter(QName qName, String packageName) throws WriteException {
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

    public <T> T read( Class<T> docClass, InputStream inputStream ) throws WriteException {
        JAXBElement<T> doc = null;
        Unmarshaller u = null;
        try {
            u = context.createUnmarshaller();
            doc = (JAXBElement<T>)u.unmarshal( inputStream );
        } catch (JAXBException e) {
            throw new WriteException(e);
        }
        return doc.getValue();
    }
}
