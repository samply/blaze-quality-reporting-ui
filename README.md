# Blaze Quality Reporting UI

## Public Demo

https://blaze.life.uni-leipzig.de/quality-reporting

Please be aware that your browser might not allow to access a Blaze server over insecure HTTP. Please use the Docker container in case you get network errors.

## Usage

Start the container:
```
docker run -p 8000:80 samply/blaze-quality-reporting-ui:0.7.2
```

Open in your browser:
```
http://localhost:8000
```

## Development

```bash
npm start
```

Open in your browser:
```
http://localhost:3000
```

### Production Build

```bash
npm run prod
docker build -t samply/blaze-quality-reporting-ui:0.7.2
```

## License

Copyright 2019 The Samply Development Community

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
