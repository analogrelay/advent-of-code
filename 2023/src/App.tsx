import { RouteObject, RouterProvider, createBrowserRouter } from 'react-router-dom';
import './App.css';
import Home from './Home';
import Day from './Day';

const dayRoutes: RouteObject[] = Array.from({ length: 25 }, (_, i) => ({
  path: `days/${i + 1}`,
  Component: () => <Day day={i + 1} />,
}));

const routes: RouteObject[] = [
  {
    path: "/",
    Component: () => <Home />,
  },
  ...dayRoutes,
]

const router = createBrowserRouter(routes);

function Fallback() {
  return <p className="italic text-center">Loading...</p>
}

function App() {
  return (
    <div className="grid h-screen w-screen bg-gradient-to-br from-slate-900 to-slate-500">
      <div className="m-10 p-4 bg-slate-100/80 backdrop-blur-sm rounded">
        <h1 className="font-semibold text-2xl text-center">Advent of Code 2023</h1>
        <RouterProvider router={router} fallbackElement={<Fallback />} />
      </div>
    </div>
  );
}

export default App;
