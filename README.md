## Test


| Column Name     | Data Type | Description                                                                 |
|------------------|------------|-----------------------------------------------------------------------------|
| `id`             | Integer    | Unique identifier for each record.                                         |
| `timestamp`      | Datetime   | Date and time the data was recorded (ISO 8601 format).                     |
| `sensor_value`   | Float      | The raw reading from the sensor device.                                    |
| `device_id`      | String     | Unique identifier of the device collecting the data.                       |
| `status`         | String     | Operational status of the sensor (`active`, `inactive`, or `error`).      |
| `location`       | String     | Geographic location or label of the sensor’s position.                    |
