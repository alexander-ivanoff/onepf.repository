package org.onepf.repository.api.responsewriter;

/**
 *
 * Created by ivanoff on 19.03.14.
 */
public interface WritableHeader {


    void writeOpening(ResponseWriter responseWriter) throws WriteException;
    void writeClosing(ResponseWriter responseWriter) throws WriteException;
}
