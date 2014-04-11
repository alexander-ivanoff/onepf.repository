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

    private static final String PROPERTY_DBURL= "dbUrl";
    private static final String PROPERTY_DBDRIVER= "dbDriver";
    private static final String PROPERTY_DBUSER= "dbUser";
    private static final String PROPERTY_DBPASSWORD= "dbPassword";

    // sql connection options
    public String driverClassName = null;
    public String dbUser = null;
    public String dbPassword = null;
    public String dbUrl = null; // must be overridden in web.xml
    public int maxConnections = 4;
    public int maxWait = 10000; // 10sec

    public SqlOptions(Properties props) {
        driverClassName = props.getProperty(PROPERTY_DBDRIVER);
        dbUrl = props.getProperty(PROPERTY_DBURL);
        dbUser = props.getProperty(PROPERTY_DBUSER);
        dbPassword = props.getProperty(PROPERTY_DBPASSWORD);
        if (driverClassName == null || dbUrl == null || dbUser == null || dbPassword == null) {
            throw new IllegalArgumentException("configuration file is not completed");
        }
    }


    @Override
    public DataService createDataService() {
        return new SqlDataService(this);
    }
}
