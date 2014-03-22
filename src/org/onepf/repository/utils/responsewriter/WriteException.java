package org.onepf.repository.utils.responsewriter;

/**
 * Created by ivanoff on 19.03.14.
 */
public class WriteException extends Exception {

    WriteException(Exception originalException) {
        super(originalException);
    }
}
