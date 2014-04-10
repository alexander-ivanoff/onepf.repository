package org.onepf.repository.model.services.mysql;

import org.onepf.repository.model.services.DataService;
import org.onepf.repository.model.services.DataServiceOptions;

/**
 *
 * Realization of DataServiceOptions for Mysql
 *
 * @author Alexander Ivanoff on 25.03.14.
 */
public class SqlOptions implements DataServiceOptions {

    // sql connection options
    public String driverClassName = "com.mysql.jdbc.Driver";
    public String dbUser = "onepf_user";
    public String dbPassword = "onepfPassword";
    public String dbUrl = "jdbc:mysql://onepf-repository.ca6s4oee7dbi.us-west-2.rds.amazonaws.com:3306/onepf_repository_1"; // must be overridden in web.xml
    public int maxConnections = 4;
    public int maxWait = 10000; // 10sec


    @Override
    public DataService createDataService() {
        return new SqlDataService(this);
    }
}
