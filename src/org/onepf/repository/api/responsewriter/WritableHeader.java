package org.onepf.repository.api.responsewriter;

/**
 * Interface used as enclosing list element.
 *
 * @author Alexander Ivanoff on 19.03.14.
 */
public interface WritableHeader {

    /**
     * write opening elements to a response with provided responseWriter
     *
     * @param responseWriter
     * @throws WriteException
     */
    void writeOpening(ResponseWriter responseWriter) throws WriteException;

    /**
     * write closing elements to a response with provided responseWriter
     *
     * @param responseWriter
     * @throws WriteException
     */
    void writeClosing(ResponseWriter responseWriter) throws WriteException;
}
