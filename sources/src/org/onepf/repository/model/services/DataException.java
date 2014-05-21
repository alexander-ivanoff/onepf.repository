package org.onepf.repository.model.services;

/**
 *
 * This exception can be thrown by DataService.
 *
 * @see org.onepf.repository.model.services.DataService
 * @author Alexander Ivanoff
 */
public class DataException extends Exception {

    private Error error;

    public DataException(Exception e) {
        this(Error.GENERIC, e);
    }

    public DataException(Error e) {
        this(e, null);
    }

    public DataException(Error error, Exception e) {
        super(e);
        setError(error);
    }



    public Error getError() {
        return error;
    }

    public void setError(Error error) {
        this.error = error;
    }
}
