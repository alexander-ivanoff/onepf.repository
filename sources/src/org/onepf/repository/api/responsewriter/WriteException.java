package org.onepf.repository.api.responsewriter;

/**
 * This exeption can be thrown in process of response creation
 *
 * @author Alexander Ivanoff
 */
public class WriteException extends Exception {

    public WriteException(Exception originalException) {
        super(originalException);
    }
}
