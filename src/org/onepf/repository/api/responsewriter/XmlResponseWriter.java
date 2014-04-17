package org.onepf.repository.api.responsewriter;

import org.onepf.repository.api.responsewriter.descriptors.*;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.responsewriter.entity.DownloadEntity;
import org.onepf.repository.api.responsewriter.entity.PurchaseEntity;
import org.onepf.repository.api.responsewriter.entity.ReviewEntity;
import org.onepf.repository.api.xmlapi.*;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.Writer;
import java.util.List;

/**
 * ResponseWriter realization to create responses in xml format.
 *
 * @author Alexander Ivanoff
 */
public class XmlResponseWriter extends ResponseWriter {

    XMLStreamWriter out = null;


    @Override
    public void write(Writer writer, WritableHeader header, List<? extends Object> items) throws WriteException {
        try {
            out = XMLOutputFactory.newInstance().createXMLStreamWriter(writer);
            out.writeStartDocument("UTF-8", "1.0");
            header.writeOpening(this);
            for (Object writable : items) {
                ((Writable) writable).write(this);
            }
            header.writeClosing(this);
            out.writeEndDocument();
            out.flush();
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        } finally {
             if (out != null) {
                 try {
                     out.close();
                     out = null;
                 } catch (XMLStreamException e) {
                     throw new WriteException(e);
                 }
             }
        }

    }

    @Override
    public void write(ApplicationEntity app) throws WriteException{
        try {
            out.writeEmptyElement(XMLElements.Application.ELEMENT_NAME);
            out.writeAttribute(XMLElements.Application.FIELD_PACKAGE, app.getPackageName());
            out.writeAttribute(XMLElements.Application.FIELD_HASH, app.getAppdfHash());
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    @Override
    public void write(PurchaseEntity purchase) throws WriteException {
        try {
            out.writeStartElement("purchase");  //Move all strings to XMLElements
//            writeElement(out, "id", purchase.getId());
            writeElement(out, "package", purchase.getPackageName() );
            writeElement(out, "datetime", String.valueOf(purchase.getDateTime()));
            writeElement(out, "version", purchase.getVersion());
            writeElement(out, "build", String.valueOf(purchase.getBuild()));
            writeElement(out, "last-updated", String.valueOf(purchase.getLastUpdate()));
            writeElement(out, "device-model", purchase.getDeviceModel());
            writeElement(out, "device-name", purchase.getDeviceName());
            writeElement(out, "country", purchase.getCountry());
            writeElement(out, "user-price", purchase.getUserPrice());
            writeElement(out, "user-currency", purchase.getUserCurrency());
            writeElement(out, "inner-price", purchase.getInnerPrice());
            writeElement(out, "inner-currency", purchase.getInnerCurrency());
            out.writeEndElement();
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    @Override
    public void write(DownloadEntity download) throws WriteException {
        try {
            out.writeStartElement("download");  //Move all strings to XMLElements
            writeElement(out, "package", download.getPackageName() );
            writeElement(out, "datetime", String.valueOf(download.getDateTime()));
            writeElement(out, "version", download.getVersion());
            writeElement(out, "build", String.valueOf(download.getBuild()));
            writeElement(out, "last-updated", String.valueOf(download.getLastUpdate()));
            writeElement(out, "device-model", download.getDeviceModel());
            writeElement(out, "device-name", download.getDeviceName());
            writeElement(out, "country", download.getCountry());
            writeElement(out, "is-update", download.getIsUpdate());
            out.writeEndElement();
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    @Override
    public void write(ReviewEntity review) throws WriteException {
        try {
            out.writeStartElement("download"); //Move all strings to XMLElements
            writeElement(out, "package", review.getPackageName() );
            writeElement(out, "version", review.getVersion());
            writeElement(out, "build", String.valueOf(review.getBuild()));
//            writeElement(out, "last-updated", String.valueOf(revie));
            writeElement(out, "device-model", review.getDeviceModel());
            writeElement(out, "device-name", review.getDeviceName());
            writeElement(out, "country", review.getCountry());
            writeElement(out, "stars", String.valueOf(review.getRating()));
            writeElement(out, "userName", review.getUserName());
            writeElement(out, "title", review.getTitle());
            writeElement(out, "text", review.getBody());
            out.writeEndElement();
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    /**
     * writes element with characters, like <elementName>elementValue</elementName>
     *
     * @param out - xml writer to write into
     * @param elementName - name of the element
     * @param elementValue - characters used as element body
     * @throws XMLStreamException
     */
    private static void writeElement(XMLStreamWriter out, String elementName, String elementValue) throws XMLStreamException {
        out.writeStartElement(elementName);
        out.writeCharacters(elementValue);
        out.writeEndElement();
    }

    @Override
    public void writeOpening(ApplicationListHeaderDescriptor descriptor) throws WriteException {
//        writeOpeningInt(XMLElements.ApplicationListHeader.ELEMENT_NAME, descriptor);
    }

    @Override
    public void writeOpening(DownloadsListHeaderDescriptor descriptor) throws WriteException {
        writeOpeningInt(XMLElements.DownloadsListHeader.ELEMENT_NAME, descriptor);
    }

    @Override
    public void writeOpening(PurchasesListHeaderDescriptor descriptor) throws WriteException {
        writeOpeningInt(XMLElements.PurchasesListHeader.ELEMENT_NAME, descriptor);
    }

    @Override
    public void writeOpening(ReviewsListHeaderDescriptor descriptor) throws WriteException {
        writeOpeningInt(XMLElements.ReviewsListHeader.ELEMENT_NAME, descriptor);
    }

    @Override
    public void writeClosing(ApplicationListHeaderDescriptor descriptor) throws WriteException {
//        writeClosingInt(descriptor);
    }

    @Override
    public void writeClosing(DownloadsListHeaderDescriptor descriptor) throws WriteException {
        writeClosingInt(descriptor);
    }

    @Override
    public void writeClosing(PurchasesListHeaderDescriptor descriptor) throws WriteException {
        writeClosingInt(descriptor);
    }

    @Override
    public void writeClosing(ReviewsListHeaderDescriptor descriptor) throws WriteException {
        writeClosingInt(descriptor);
    }

    /**
     * Internal method to write generic list opening elements, similair for all xml responses
     *
     * @param name - name of the opening element
     * @param baseListHeaderDescriptor - base header descriptor. contains protocol version & offset to the next page
     * @throws WriteException
     */
    private void writeOpeningInt(String name, BaseListHeaderDescriptor baseListHeaderDescriptor) throws WriteException {
        try {
            out.writeStartElement(name);
            if (baseListHeaderDescriptor.version != null) {
                out.writeAttribute(XMLElements.BaseHeader.FIELD_VERSION, baseListHeaderDescriptor.version);
            }
            if (baseListHeaderDescriptor.offset != null) {
                out.writeAttribute(XMLElements.BaseHeader.FIELD_OFFSET, baseListHeaderDescriptor.offset);
            }
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    /**
     * Internal method to write generic elements closing a list, similair for all xml responses.
     *
     * @param baseListHeaderDescriptor
     * @throws WriteException
     */
    private void writeClosingInt(BaseListHeaderDescriptor baseListHeaderDescriptor) throws WriteException {
        try {
            out.writeEndElement();
        } catch (XMLStreamException e) {
            throw  new WriteException(e);
        }
    }


}
