package org.onepf.repository.api.responsewriter;

import org.onepf.repository.api.responsewriter.descriptors.*;
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
    public void write(Writer writer, WritableHeader header, List<? extends Writable> items) throws WriteException {
        try {
            out = XMLOutputFactory.newInstance().createXMLStreamWriter(writer);
            out.writeStartDocument("UTF-8", "1.0");
            header.writeOpening(this);
            for (Writable writable : items) {
                writable.write(this);
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
    public void write(ApplicationDescriptor app) throws WriteException{
        try {
            out.writeEmptyElement(XMLElements.Application.ELEMENT_NAME);
            out.writeAttribute(XMLElements.Application.FIELD_PACKAGE, app.packageName);
            out.writeAttribute(XMLElements.Application.FIELD_HASH, app.appdfHash);
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    @Override
    public void write(PurchaseDescriptor purchase) throws WriteException {
        try {
            out.writeStartElement("purchase");  //Move all strings to XMLElements
            writeElement(out, "id", purchase.id);
            writeElement(out, "package", purchase.packageName );
            writeElement(out, "datetime", String.valueOf(purchase.dateTime));
            writeElement(out, "version", purchase.version);
            writeElement(out, "build", String.valueOf(purchase.build));
            writeElement(out, "last-updated", String.valueOf(purchase.lastUpdate));
            writeElement(out, "device-model", purchase.deviceModel);
            writeElement(out, "device-name", purchase.deviceName);
            writeElement(out, "country", purchase.country);
            writeElement(out, "user-price", purchase.userPrice);
            writeElement(out, "user-currency", purchase.userCurrency);
            writeElement(out, "inner-price", purchase.innerPrice);
            writeElement(out, "inner-currency", purchase.innerCurrency);
            out.writeEndElement();
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    @Override
    public void write(DownloadDescriptor download) throws WriteException {
        try {
            out.writeStartElement("download");  //Move all strings to XMLElements
            writeElement(out, "package", download.packageName );
            writeElement(out, "datetime", String.valueOf(download.dateTime));
            writeElement(out, "version", download.version);
            writeElement(out, "build", String.valueOf(download.build));
            writeElement(out, "last-updated", String.valueOf(download.lastUpdate));
            writeElement(out, "device-model", download.deviceModel);
            writeElement(out, "device-name", download.deviceName);
            writeElement(out, "country", download.country);
            writeElement(out, "is-update", download.isUpdate);
            out.writeEndElement();
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    @Override
    public void write(ReviewDescriptor review) throws WriteException {
        try {
            out.writeStartElement("download"); //Move all strings to XMLElements
            writeElement(out, "package", review.packageName );
            writeElement(out, "version", review.version);
            writeElement(out, "build", String.valueOf(review.build));
            writeElement(out, "last-updated", String.valueOf(review.lastUpdate));
            writeElement(out, "device-model", review.deviceModel);
            writeElement(out, "device-name", review.deviceName);
            writeElement(out, "country", review.country);
            writeElement(out, "stars", String.valueOf(review.stars));
            writeElement(out, "userName", review.userName);
            writeElement(out, "title", review.title);
            writeElement(out, "text", review.body);
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
        writeOpeningInt(XMLElements.ApplicationListHeader.ELEMENT_NAME, descriptor);
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
        writeClosingInt(descriptor);
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
