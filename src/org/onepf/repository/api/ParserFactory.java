package org.onepf.repository.api;

import org.onepf.repository.api.responsewriter.descriptors.ApplicationListHeaderDescriptor;
import org.onepf.repository.api.responsewriter.entity.ApplicationEntity;
import org.onepf.repository.api.xmlapi.*;

import java.io.InputStream;
import java.util.Collection;

/**
 * Abstraction to parse api objects from Input Stream.
 *
 * @author Alexander Ivanoff
 *
 * @see org.onepf.repository.api.ListParser
 */
public abstract class ParserFactory<T extends ListParser> {


    /**
     * Can be thrown in parsing process
     */
    public static class ParseException extends Exception {

        public ParseException(Exception e) {
            super(e);
        }
    }

    /**
     * @return parser factory realization for xml api
     */
    public static ParserFactory getXmlParserFactory() {
        return new XmlParserFactory();
    }

    /**
     * parse content contains list of objects
     *
     * @param parser - ListParser object
     * @param is
     * @return - MD5 calculated hash of the content
     * @throws ParseException
     */
    public abstract String parse(T parser, InputStream is) throws ParseException;

    /**
     *
     * @param apps - collection to store parsed ApplicationDescriptors
     * @return ListParser to parse list of ApplicationsDescriptor into collection provided in parameter
     */
    public abstract ListParser<ApplicationEntity, ApplicationListHeaderDescriptor> getApplicationParser(Collection<ApplicationEntity> apps);
}
