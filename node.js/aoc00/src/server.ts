import * as express from 'express';

// Constants
const PORT = 3000;
const HOST = '0.0.0.0';

// App
const app = express();
app.get('/', (req: any, res: any) => {
	res.send('Hello remote world!\n');
});

app.listen(PORT, HOST);
console.log(`Running on http://${HOST}:${PORT}`);

// Used for automated testing
if (process.env.REGRESSION_TESTING === 'true') { process.exit(0); }
