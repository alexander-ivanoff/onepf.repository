package org.onepf.repository.utils.responsewriter;

import org.onepf.repository.utils.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.DownloadDescriptor;
import org.onepf.repository.utils.responsewriter.descriptors.PurchaseDescriptor;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.util.List;
import java.util.Map;

/**
 * Created by ivanoff on 12.03.14.
 */
public class XmlResponseWriter extends ResponseWriter {

    XMLStreamWriter out = null;

    @Override
    public void write(java.io.Writer writer, String name, Map<String, String> headers, List<? extends Writable> listOfWritables) throws WriteException {

        try {
             out = XMLOutputFactory.newInstance().createXMLStreamWriter(writer);

            out.writeStartDocument("UTF-8", "1.0");
            out.writeStartElement(name);
            if (headers != null) {
                for (Map.Entry<String, String> entry : headers.entrySet()) {
                    out.writeAttribute(entry.getKey(), entry.getValue());
                }
            }

            for (Writable writable : listOfWritables) {
                writable.write(this);
            }
            out.writeEndElement();
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
            out.writeEmptyElement("application");
            out.writeAttribute("package", app.packageName);
            //out.writeAttribute("version", app.version);
            //out.writeAttribute("build", String.valueOf(app.build));
            out.writeAttribute("last-updated", String.valueOf(app.lastUpdated));
        } catch (XMLStreamException e) {
            throw new WriteException(e);
        }
    }

    @Override
    public void write(PurchaseDescriptor purchase) throws WriteException {
        try {
            out.writeStartElement("purchase");
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
            out.writeStartElement("download");
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

    private static void writeElement(XMLStreamWriter out, String elementName, String elementValue) throws XMLStreamException {
        out.writeStartElement(elementName);
        out.writeCharacters(elementValue);
        out.writeEndElement();
    }

}
