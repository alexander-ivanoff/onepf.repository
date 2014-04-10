package org.onepf.repository.api.responsewriter;

/**
 * Interface used as list item element.
 *
 * @author Alexander Ivanoff on 19.03.14.
 */
public interface Writable {

    /**
     * write list element to a response with provided responseWriter
     *
     * @param responseWriter
     * @throws WriteException
     */
    void write(ResponseWriter responseWriter) throws WriteException;
}
