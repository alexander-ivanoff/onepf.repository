package org.onepf.repository.api;

import org.onepf.repository.api.responsewriter.descriptors.ApplicationDescriptor;
import org.onepf.repository.api.responsewriter.descriptors.ApplicationListHeaderDescriptor;
import org.onepf.repository.api.xmlapi.*;

import java.io.InputStream;
import java.util.Collection;

/**
 * Created by ivanoff on 03.04.14.
 */
public abstract class ParserFactory<T extends ListParser> {



    public static class ParseException extends Exception {

        public ParseException(Exception e) {
            super(e);
        }
    }

    public static ParserFactory getXmlParserFactory() {
        return new XmlParserFactory();
    }

    public abstract String parse(T parser, InputStream is) throws ParseException;

    public abstract ListParser<ApplicationDescriptor, ApplicationListHeaderDescriptor> getApplicationParser(Collection<ApplicationDescriptor> apps);
}
