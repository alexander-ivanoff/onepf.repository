package org.onepf.repository.model.services.mysql;

import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.DataServiceOptions;

import java.util.Properties;

/**
 *
 * Realization of DataServiceOptions for Mysql
 *
 * @author Alexander Ivanoff on 25.03.14.
 */
public class SqlOptions implements DataServiceOptions {

    public static final String SERVICE_NAME= "mysql";

    private static final String PROPERTY_HIBERNATE_SETTING_FILE= "sql_hibernate_settings";

    // sql connection options
    public String hibernateSettingFile;

    public SqlOptions(Properties props) {
        hibernateSettingFile = props.getProperty(PROPERTY_HIBERNATE_SETTING_FILE);
        if (hibernateSettingFile == null) {
            throw new IllegalArgumentException("configuration file is not completed");
        }
    }


    @Override
    public DataService createDataService() {
        return new SqlDataService(this);
    }
}
