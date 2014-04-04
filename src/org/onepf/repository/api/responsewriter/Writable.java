package org.onepf.repository.api.responsewriter;

/**
 *
 * Created by ivanoff on 19.03.14.
 */
public interface Writable {


    void write(ResponseWriter responseWriter) throws WriteException;
}
