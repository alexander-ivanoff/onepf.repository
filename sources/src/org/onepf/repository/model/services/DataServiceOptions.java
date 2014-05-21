package org.onepf.repository.model.services;

/**
 * Abstraction of options object to create and configure data service
 *
* @author Alexander Ivanoff on 07.04.14.
 */
public interface DataServiceOptions {

    /**
     * Create data service with given options
     * @return created data service
     *
     * @see org.onepf.repository.model.services.DataService
     */
    DataService createDataService();
}
