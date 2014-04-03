package org.onepf.repository.xmlapi;

import org.onepf.repository.utils.responsewriter.descriptors.AbstractDescriptor;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.util.Collection;

/**
 * Created by ivanoff on 01.04.14.
 */
public class XmlListParser<T extends AbstractDescriptor, K extends  AbstractDescriptor> extends DefaultHandler {

    Collection<T> items;
    K header;

    XmlItemParser<T> itemParser;
    XmlItemParser<K> headerParser;

    public XmlListParser(Collection<T> items, XmlItemParser<T> itemParser, XmlItemParser<K> headerParser) {
        this.items = items;
        this.itemParser = itemParser;
        this.headerParser = headerParser;
    }

    @Override
    public void startDocument() throws SAXException {

    }


    @Override
    public void endDocument() throws SAXException {

    }

    @Override
    public void characters(char[] chars, int start, int end) throws SAXException {
        if (headerParser.getDescriptor() != null) {
            headerParser.characters(chars, start, end);
        } else if (itemParser.getDescriptor() != null) {
            itemParser.characters(chars, start, end);
        }
    }

    @Override
    public void startElement(String uri, String localName, String qName, Attributes attributes) throws SAXException {
        if (qName.equals(headerParser.getElementName())) {
            headerParser.startElement(uri, localName, qName, attributes);
        } else if (qName.equals(itemParser.getElementName())) {
            itemParser.startElement(uri, localName, qName, attributes);
        }
    }

    @Override
    public void endElement(String uri, String localName, String qName) throws SAXException {
        if (qName.equals(headerParser.getElementName())) {
            headerParser.endElement(uri, localName, qName);
            header = headerParser.removeDescriptor();
        } else if (qName.equals(itemParser.getElementName())) {
            itemParser.endElement(uri, localName, qName);
            items.add(itemParser.removeDescriptor());
        }
    }

    public Collection<T> getItems() {
        return items;
    }

    public K getHeader() {
        return header;
    }



}
