package org.onepf.repository.model.services;

/**
 *
 * This exception can be thrown by DataService.
 *
 * @see org.onepf.repository.model.services.DataService
 * @author Alexander Ivanoff
 */
public class DataException extends Exception {

    public DataException(Exception e) {
        super(e);
    }

    public DataException(String message) {
        super(message);
    }
}
